use serde::{Serialize, Deserialize};
use actix_web::{HttpResponse, HttpRequest, Responder, Error};
use futures::future::{ready, Ready};
use sqlx::{PgPool, FromRow, Row};
use sqlx::postgres::PgRow;
use anyhow::Result;

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct SubseriesRequest {
    pub synopsis: Option<String>,
    pub title: String,
    pub total_book_members: i32,
    pub series_id: i64,
}

#[derive(Serialize, Deserialize, FromRow, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Subseries {
    pub id: i64,
    pub synopsis: Option<String>,
    pub title: String,
    pub total_book_members: i32,
    pub series_id: i64,
}

impl Responder for Subseries {
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

impl Subseries {
    pub async fn find_all(pool: &PgPool) -> Result<Vec<Subseries>> {
        let mut xs = vec![];
        let recs = sqlx::query!(
            r#"
                SELECT id, synopsis, title, total_book_members, series_id
                    FROM subseries
                ORDER BY id
            "#
            )
                .fetch_all(pool)
                .await?;

        for r in recs {
            xs.push(Subseries {
                id: r.id,
                synopsis: r.synopsis,
                title: r.title,
                total_book_members: r.total_book_members,
                series_id: r.series_id,
            });
        }

        Ok(xs)
    }

    pub async fn find_by_id(id: i64, pool: &PgPool) -> Result<Subseries> {
        let r = sqlx::query!(
            r#"
                SELECT * FROM subseries WHERE id = $1
            "#,
            id
            )
            .fetch_one(&*pool)
            .await?;

        Ok(Subseries {
            id: r.id,
            synopsis: r.synopsis,
            title: r.title,
            total_book_members: r.total_book_members,
            series_id: r.series_id,
        })
    }

    pub async fn create(subseries: SubseriesRequest, pool: &PgPool) -> Result<Subseries> {
        let mut tx = pool.begin().await?;
        let subseries = sqlx::query("INSERT INTO subseries (synopsis, title, total_book_members, series_id) VALUES ($1, $2, $3, $4) RETURNING id, synopsis, title, total_book_members, series_id")
            .bind(&subseries.synopsis)
            .bind(&subseries.title)
            .bind(subseries.total_book_members)
            .bind(subseries.series_id)
            .map(|row: PgRow| {
                Subseries {
                    id: row.get(0),
                    synopsis: row.get(1),
                    title: row.get(2),
                    total_book_members: row.get(3),
                    series_id: row.get(4),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(subseries)
    }

    pub async fn update(id: i64, subseries: SubseriesRequest, pool: &PgPool) -> Result<Subseries> {
        let mut tx = pool.begin().await.unwrap();
        let subseries = sqlx::query("UPDATE subseries SET synopsis = $2, title = $3, total_book_members = $4, series_id = $5 WHERE id = $1 RETURNING id, synopsis, title, total_book_members series_id")
            .bind(id)
            .bind(&subseries.synopsis)
            .bind(&subseries.title)
            .bind(subseries.total_book_members)
            .bind(subseries.series_id)
            .map(|row: PgRow| {
                Subseries {
                    id: row.get(0),
                    synopsis: row.get(1),
                    title: row.get(2),
                    total_book_members: row.get(3),
                    series_id: row.get(4),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        Ok(subseries)
    }

    pub async fn delete(id: i64, pool: &PgPool) -> Result<i64> {
        let mut tx = pool.begin().await?;
        sqlx::query("DELETE FROM subseries WHERE id = $1")
            .bind(id)
            .execute(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(id)
    }
}
