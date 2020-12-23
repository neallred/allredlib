use serde::{Serialize, Deserialize};
use actix_web::{HttpResponse, HttpRequest, Responder, Error};
use futures::future::{ready, Ready};
use sqlx::{PgPool, FromRow, Row};
use sqlx::postgres::PgRow;
use anyhow::Result;

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct TitleRequest {
    pub title: String,
    pub year: Option<i32>,
    pub synopsis: Option<String>,
    pub series_part: Option<String>,
    pub series_id: Option<i64>,
    pub subseries_part: Option<String>,
    pub subseries_id: Option<i64>,
}

#[derive(Serialize, Deserialize, FromRow, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Title {
    pub id: i64,
    pub title: String,
    pub year: Option<i32>,
    pub synopsis: Option<String>,
    pub series_part: Option<String>,
    pub series_id: Option<i64>,
    pub subseries_part: Option<String>,
    pub subseries_id: Option<i64>,
}

impl Responder for Title {
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

impl Title {
    pub async fn find_all(pool: &PgPool) -> Result<Vec<Title>> {
        let mut xs = vec![];
        let recs = sqlx::query!(
            r#"
                SELECT id, title, year, synopsis, series_part, series_id, subseries_part, subseries_id
                    FROM titles
                ORDER BY id
            "#
            )
                .fetch_all(pool)
                .await?;

        for r in recs {
            xs.push(Title {
                id: r.id,
                title: r.title,
                year: r.year,
                synopsis: r.synopsis,
                series_part: r.series_part,
                series_id: r.series_id,
                subseries_part: r.subseries_part,
                subseries_id: r.subseries_id,
            });
        }

        Ok(xs)
    }

    pub async fn find_by_id(id: i64, pool: &PgPool) -> Result<Title> {
        let r = sqlx::query!(
            r#"
                SELECT * FROM titles WHERE id = $1
            "#,
            id
            )
            .fetch_one(&*pool)
            .await?;

        Ok(Title {
            id: r.id,
            title: r.title,
            year: r.year,
            synopsis: r.synopsis,
            series_part: r.series_part,
            series_id: r.series_id,
            subseries_part: r.subseries_part,
            subseries_id: r.subseries_id,
        })
    }

    pub async fn create(title: TitleRequest, pool: &PgPool) -> Result<Title> {
        let mut tx = pool.begin().await?;
        let title = sqlx::query("INSERT INTO titles (title, year, synopsis, series_part, series_id, subseries_part, subseries_id) VALUES ($1, $2, $3, $4, $5, $6, $7) RETURNING id, title, year, synopsis, series_part, series_id, subseries_part, subseries_id")
            .bind(&title.title)
            .bind(title.year)
            .bind(&title.synopsis)
            .bind(&title.series_part)
            .bind(title.series_id)
            .bind(&title.subseries_part)
            .bind(title.subseries_id)
            .map(|row: PgRow| {
                Title {
                    id: row.get(0),
                    title: row.get(1),
                    year: row.get(2),
                    synopsis: row.get(3),
                    series_part: row.get(4),
                    series_id: row.get(5),
                    subseries_part: row.get(6),
                    subseries_id: row.get(7),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(title)
    }

    pub async fn update(id: i64, title: TitleRequest, pool: &PgPool) -> Result<Title> {
        let mut tx = pool.begin().await.unwrap();
        let title = sqlx::query("UPDATE titles SET title = $2, year = $3, synopsis = $4, series_part = $5, series_id = $6, subseries_part = $7, subseries_id = $8 WHERE id = $1 RETURNING id, title, year, synopsis, series_part, series_id, subseries_part, subseries_id")
            .bind(id)
            .bind(&title.title)
            .bind(title.year)
            .bind(&title.synopsis)
            .bind(&title.series_part)
            .bind(title.series_id)
            .bind(&title.subseries_part)
            .bind(title.subseries_id)
            .map(|row: PgRow| {
                Title {
                    id: row.get(0),
                    title: row.get(1),
                    year: row.get(2),
                    synopsis: row.get(3),
                    series_part: row.get(4),
                    series_id: row.get(5),
                    subseries_part: row.get(6),
                    subseries_id: row.get(7),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        Ok(title)
    }

    pub async fn delete(id: i64, pool: &PgPool) -> Result<i64> {
        let mut tx = pool.begin().await?;
        sqlx::query("DELETE FROM titles WHERE id = $1")
            .bind(id)
            .execute(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(id)
    }
}
