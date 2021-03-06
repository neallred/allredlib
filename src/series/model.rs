use serde::{Serialize, Deserialize};
use actix_web::{HttpResponse, HttpRequest, Responder, Error};
use futures::future::{ready, Ready};
use sqlx::{PgPool, FromRow, Row};
use sqlx::postgres::PgRow;
use anyhow::Result;

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct SeriesRequest {
    pub synopsis: Option<String>,
    pub title: String,
    pub total_book_members: i32,
    pub total_subseries: i32,
}

#[derive(Serialize, Deserialize, FromRow, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Series {
    pub id: i64,
    pub synopsis: Option<String>,
    pub title: String,
    pub total_book_members: i32,
    pub total_subseries: i32,
}

impl Responder for Series {
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

impl Series {
    pub async fn find_all(pool: &PgPool) -> Result<Vec<Series>> {
        let mut xs = vec![];
        let recs = sqlx::query!(
            r#"
                SELECT id, synopsis, title, total_book_members, total_subseries
                    FROM series
                ORDER BY id
            "#
            )
                .fetch_all(pool)
                .await?;

        for r in recs {
            xs.push(Series {
                id: r.id,
                synopsis: r.synopsis,
                title: r.title,
                total_book_members: r.total_book_members,
                total_subseries: r.total_subseries,
            });
        }

        Ok(xs)
    }

    pub async fn find_by_id(id: i64, pool: &PgPool) -> Result<Series> {
        let r = sqlx::query!(
            r#"
                SELECT * FROM series WHERE id = $1
            "#,
            id
            )
            .fetch_one(&*pool)
            .await?;

        Ok(Series {
            id: r.id,
            synopsis: r.synopsis,
            title: r.title,
            total_book_members: r.total_book_members,
            total_subseries: r.total_subseries,
        })
    }

    pub async fn create(series: SeriesRequest, pool: &PgPool) -> Result<Series> {
        let mut tx = pool.begin().await?;
        let series = sqlx::query("INSERT INTO series (synopsis, title, total_book_members, total_subseries) VALUES ($1, $2, $3, $4) RETURNING id, synopsis, title, total_book_members, total_subseries")
            .bind(&series.synopsis)
            .bind(&series.title)
            .bind(series.total_book_members)
            .bind(series.total_subseries)
            .map(|row: PgRow| {
                Series {
                    id: row.get(0),
                    synopsis: row.get(1),
                    title: row.get(2),
                    total_book_members: row.get(3),
                    total_subseries: row.get(4),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(series)
    }

    pub async fn update(id: i64, series: SeriesRequest, pool: &PgPool) -> Result<Series> {
        let mut tx = pool.begin().await.unwrap();
        let series = sqlx::query("UPDATE series SET synopsis = $2, title = $3, total_book_members = $4, total_subseries = $5 WHERE id = $1 RETURNING id, synopsis, title, total_book_members, total_subseries")
            .bind(id)
            .bind(&series.synopsis)
            .bind(&series.title)
            .bind(series.total_book_members)
            .bind(series.total_subseries)
            .map(|row: PgRow| {
                Series {
                    id: row.get(0),
                    synopsis: row.get(1),
                    title: row.get(2),
                    total_book_members: row.get(3),
                    total_subseries: row.get(4),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        Ok(series)
    }

    pub async fn delete(id: i64, pool: &PgPool) -> Result<i64> {
        let mut tx = pool.begin().await?;
        sqlx::query("DELETE FROM series WHERE id = $1")
            .bind(id)
            .execute(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(id)
    }
}
