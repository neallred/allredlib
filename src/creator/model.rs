use serde::{Serialize, Deserialize};
use actix_web::{HttpResponse, HttpRequest, Responder, Error};
use futures::future::{ready, Ready};
use sqlx::{PgPool, FromRow, Row};
use sqlx::postgres::PgRow;
use anyhow::Result;

#[derive(Serialize, Deserialize, Debug)]
pub struct CreatorRequest {
    pub bio: Option<String>,
    pub birth: Option<i32>,
    pub death: Option<i32>,
    pub firstname: String,
    pub lastname: Option<String>,
    pub source: Option<String>,
}

#[derive(Serialize, Deserialize, FromRow, Debug)]
pub struct Creator {
    pub id: i64,
    pub bio: Option<String>,
    pub birth: Option<i32>,
    pub death: Option<i32>,
    pub firstname: String,
    pub lastname: Option<String>,
    pub source: Option<String>,
}

impl Responder for Creator {
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

impl Creator {
    pub async fn find_all(pool: &PgPool) -> Result<Vec<Creator>> {
        let mut creators = vec![];
        let recs = sqlx::query!(
            r#"
                SELECT id, bio, birth, death, firstname, lastname, source
                    FROM creators
                ORDER BY id
            "#
            )
                .fetch_all(pool)
                .await?;

        for r in recs {
            creators.push(Creator {
                id: r.id,
                bio: r.bio,
                birth: r.birth,
                death: r.death,
                firstname: r.firstname,
                lastname: r.lastname,
                source: r.source,
            });
        }

        Ok(creators)
    }

    pub async fn find_by_id(id: i64, pool: &PgPool) -> Result<Creator> {
        let r = sqlx::query!(
            r#"
                SELECT * FROM creators WHERE id = $1
            "#,
            id
            )
            .fetch_one(&*pool)
            .await?;

        Ok(Creator {
            id: r.id,
            bio: r.bio,
            birth: r.birth,
            death: r.death,
            firstname: r.firstname,
            lastname: r.lastname,
            source: r.source,
        })
    }

    pub async fn create(creator: CreatorRequest, pool: &PgPool) -> Result<Creator> {
        let mut tx = pool.begin().await?;
        let creator = sqlx::query("INSERT INTO creators (bio, birth, death, firstname, lastname, source) VALUES ($1, $2, $3, $4, $5, $6) RETURNING id, bio, birth, death, firstname, lastname, source")
            .bind(&creator.bio)
            .bind(creator.birth)
            .bind(creator.death)
            .bind(&creator.firstname)
            .bind(&creator.lastname)
            .bind(&creator.source)
            .map(|row: PgRow| {
                Creator {
                    id: row.get(0),
                    bio: row.get(1),
                    birth: row.get(2),
                    death: row.get(3),
                    firstname: row.get(4),
                    lastname: row.get(5),
                    source: row.get(6),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(creator)
    }

    pub async fn update(id: i64, creator: CreatorRequest, pool: &PgPool) -> Result<Creator> {
        let mut tx = pool.begin().await.unwrap();
        let creator = sqlx::query("UPDATE creators SET bio = $2, birth = $3, death = $4, firstname = $5, lastname = $6, source = $7 WHERE id = $1 RETURNING id, bio, birth, death, firstname, lastname, source")
            .bind(id)
            .bind(&creator.bio)
            .bind(creator.birth)
            .bind(creator.death)
            .bind(&creator.firstname)
            .bind(&creator.lastname)
            .bind(&creator.source)
            .map(|row: PgRow| {
                Creator {
                    id: row.get(0),
                    bio: row.get(1),
                    birth: row.get(2),
                    death: row.get(3),
                    firstname: row.get(4),
                    lastname: row.get(5),
                    source: row.get(6),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        Ok(creator)
    }

    pub async fn delete(id: i64, pool: &PgPool) -> Result<i64> {
        let mut tx = pool.begin().await?;
        sqlx::query("DELETE FROM creators WHERE id = $1")
            .bind(id)
            .execute(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(id)
    }
}
