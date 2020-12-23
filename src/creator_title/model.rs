use serde::{Serialize, Deserialize};
use actix_web::{HttpResponse, HttpRequest, Responder, Error};
use futures::future::{ready, Ready};
use sqlx::{PgPool, FromRow, Row};
use sqlx::postgres::PgRow;
use anyhow::Result;

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct CreatorTitleRequest {
    pub creator_id: i64,
    pub title_id: i64,
}

#[derive(Serialize, Deserialize, FromRow, Debug)]
#[serde(rename_all = "camelCase")]
pub struct CreatorTitle {
    pub id: i64,
    pub creator_id: i64,
    pub title_id: i64,
}

impl Responder for CreatorTitle {
    type Error = Error;
    type Future = Ready<Result<HttpResponse, Error>>;

    fn respond_to(self, _req: &HttpRequest) -> Self::Future {
        let body = serde_json::to_string(&self).unwrap();
        ready(Ok(
                HttpResponse::Ok()
                .content_type("application/json")
                .body(body)
                ))
    }
}

impl CreatorTitle {
    pub async fn find_all(pool: &PgPool) -> Result<Vec<CreatorTitle>> {
        let mut xs = vec![];
        let recs = sqlx::query!(
            r#"
                SELECT id, creator_id, title_id
                    FROM creator_titles
                ORDER BY id
            "#
            )
                .fetch_all(pool)
                .await?;

        for r in recs {
            xs.push(CreatorTitle {
                id: r.id,
                creator_id: r.creator_id,
                title_id: r.title_id,
            });
        }

        Ok(xs)
    }

    pub async fn find_by_id(id: i64, pool: &PgPool) -> Result<CreatorTitle> {
        let r = sqlx::query!(
            r#"
                SELECT * FROM creator_titles WHERE id = $1
            "#,
            id
            )
            .fetch_one(&*pool)
            .await?;

        Ok(CreatorTitle {
            id: r.id,
            creator_id: r.creator_id,
            title_id: r.title_id,
        })
    }

    pub async fn create(creator_title: CreatorTitleRequest, pool: &PgPool) -> Result<CreatorTitle> {
        let mut tx = pool.begin().await?;
        let creator_title = sqlx::query("INSERT INTO creator_titles (creator_id, title_id) VALUES ($1, $2) RETURNING id, creator_id, title_id")
            .bind(&creator_title.creator_id)
            .bind(&creator_title.title_id)
            .map(|row: PgRow| {
                CreatorTitle {
                    id: row.get(0),
                    creator_id: row.get(1),
                    title_id: row.get(2),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(creator_title)
    }

    pub async fn update(id: i64, creator_title: CreatorTitleRequest, pool: &PgPool) -> Result<CreatorTitle> {
        let mut tx = pool.begin().await.unwrap();
        let creator_title = sqlx::query("UPDATE creator_titles SET creator_id = $2, title_id = $3 WHERE id = $1 RETURNING id, creator_id, title_id")
            .bind(id)
            .bind(&creator_title.creator_id)
            .bind(&creator_title.title_id)
            .map(|row: PgRow| {
                CreatorTitle {
                    id: row.get(0),
                    creator_id: row.get(1),
                    title_id: row.get(2),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        Ok(creator_title)
    }

    pub async fn delete(id: i64, pool: &PgPool) -> Result<i64> {
        let mut tx = pool.begin().await?;
        sqlx::query("DELETE FROM creator_titles WHERE id = $1")
            .bind(id)
            .execute(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(id)
    }
}
