use serde::{Serialize, Deserialize};
use actix_web::{HttpResponse, HttpRequest, Responder, Error};
use futures::future::{ready, Ready};
use sqlx::{PgPool, FromRow, Row};
use sqlx::postgres::PgRow;
use anyhow::Result;
use chrono::{DateTime, Utc};

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct AttributionRequest {
    pub title: String,
    pub link: Option<String>,
    pub accessed: Option<DateTime<Utc>>,
    pub year_created: Option<i32>,
    pub place_published: Option<String>,
    pub author: Option<String>,
    pub publisher: Option<String>,
}

#[derive(Serialize, Deserialize, FromRow, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Attribution {
    pub id: i64,
    pub title: String,
    pub link: Option<String>,
    pub accessed: Option<DateTime<Utc>>,
    pub year_created: Option<i32>,
    pub place_published: Option<String>,
    pub author: Option<String>,
    pub publisher: Option<String>,
}

impl Responder for Attribution {
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

impl Attribution {
    pub async fn find_all(pool: &PgPool) -> Result<Vec<Attribution>> {
        let mut xs = vec![];
        let recs = sqlx::query!(
            r#"
                SELECT id, title, link, accessed, year_created, place_published, author, publisher
                    FROM attributions
                ORDER BY id
            "#
            )
                .fetch_all(pool)
                .await?;

        for r in recs {
            xs.push(Attribution {
                id: r.id,
                title: r.title,
                link: r.link,
                accessed: r.accessed,
                year_created: r.year_created,
                place_published: r.place_published,
                author: r.author,
                publisher: r.publisher,
            });
        }

        Ok(xs)
    }

    pub async fn find_by_id(id: i64, pool: &PgPool) -> Result<Attribution> {
        let r = sqlx::query!(
            r#"
                SELECT * FROM attributions WHERE id = $1
            "#,
            id
            )
            .fetch_one(&*pool)
            .await?;

        Ok(Attribution {
            id: r.id,
            title: r.title,
            link: r.link,
            accessed: r.accessed,
            year_created: r.year_created,
            place_published: r.place_published,
            author: r.author,
            publisher: r.publisher,
        })
    }

    pub async fn create(attribution: AttributionRequest, pool: &PgPool) -> Result<Attribution> {
        let mut tx = pool.begin().await?;
        let attribution = sqlx::query("INSERT INTO attributions (title, link, accessed, year_created, place_published, author, publisher) VALUES ($1, $2, $3, $4, $5, $6, $7) RETURNING id, title, link, accessed, year_created, place_published, author, publisher")
            .bind(&attribution.title)
            .bind(&attribution.link)
            .bind(attribution.accessed)
            .bind(attribution.year_created)
            .bind(&attribution.place_published)
            .bind(&attribution.author)
            .bind(&attribution.publisher)
            .map(|row: PgRow| {
                Attribution {
                    id: row.get(0),
                    title: row.get(1),
                    link: row.get(2),
                    accessed: row.get(3),
                    year_created: row.get(4),
                    place_published: row.get(5),
                    author: row.get(6),
                    publisher: row.get(7),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(attribution)
    }

    pub async fn update(id: i64, attribution: AttributionRequest, pool: &PgPool) -> Result<Attribution> {
        let mut tx = pool.begin().await.unwrap();
        let attribution = sqlx::query("UPDATE attribution SET title = $2, link = $3, accessed = $4, year_created = $5, place_published = $6, author = $7, publisher = $7 WHERE id = $1 RETURNING id, title, link, accessed, year_created, place_published, author, publisher")
            .bind(id)
            .bind(&attribution.title)
            .bind(&attribution.link)
            .bind(attribution.accessed)
            .bind(attribution.year_created)
            .bind(&attribution.place_published)
            .bind(&attribution.author)
            .bind(&attribution.publisher)
            .map(|row: PgRow| {
                Attribution {
                    id: row.get(0),
                    title: row.get(1),
                    link: row.get(2),
                    accessed: row.get(3),
                    year_created: row.get(4),
                    place_published: row.get(5),
                    author: row.get(6),
                    publisher: row.get(7),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        Ok(attribution)
    }

    pub async fn delete(id: i64, pool: &PgPool) -> Result<i64> {
        let mut tx = pool.begin().await?;
        sqlx::query("DELETE FROM attributions WHERE id = $1")
            .bind(id)
            .execute(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(id)
    }
}
