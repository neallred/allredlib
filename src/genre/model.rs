use serde::{Serialize, Deserialize};
use actix_web::{HttpResponse, HttpRequest, Responder, Error};
use futures::future::{ready, Ready};
use sqlx::{PgPool, FromRow, Row};
use sqlx::postgres::PgRow;
use anyhow::Result;

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct GenreRequest {
    pub genre: String,
}

#[derive(Serialize, Deserialize, FromRow, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Genre {
    pub id: i64,
    pub genre: String,
}

impl Responder for Genre {
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

impl Genre {
    pub async fn find_all(pool: &PgPool) -> Result<Vec<Genre>> {
        let mut xs = vec![];
        let recs = sqlx::query!(
            r#"
                SELECT id, genre
                    FROM genres
                ORDER BY id
            "#
            )
                .fetch_all(pool)
                .await?;

        for r in recs {
            xs.push(Genre {
                id: r.id,
                genre: r.genre,
            });
        }

        Ok(xs)
    }

    pub async fn find_by_id(id: i64, pool: &PgPool) -> Result<Genre> {
        let r = sqlx::query!(
            r#"
                SELECT * FROM genres WHERE id = $1
            "#,
            id
            )
            .fetch_one(&*pool)
            .await?;

        Ok(Genre {
            id: r.id,
            genre: r.genre,
        })
    }

    pub async fn create(genre: GenreRequest, pool: &PgPool) -> Result<Genre> {
        let mut tx = pool.begin().await?;
        let genre = sqlx::query("INSERT INTO genres (genre) VALUES ($1) RETURNING id, genre")
            .bind(&genre.genre)
            .map(|row: PgRow| {
                Genre {
                    id: row.get(0),
                    genre: row.get(1),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(genre)
    }

    pub async fn update(id: i64, genre: GenreRequest, pool: &PgPool) -> Result<Genre> {
        let mut tx = pool.begin().await.unwrap();
        let genre = sqlx::query("UPDATE genres SET genre = $2 WHERE id = $1 RETURNING id, genre")
            .bind(id)
            .bind(&genre.genre)
            .map(|row: PgRow| {
                Genre {
                    id: row.get(0),
                    genre: row.get(1),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        Ok(genre)
    }

    pub async fn delete(id: i64, pool: &PgPool) -> Result<i64> {
        let mut tx = pool.begin().await?;
        sqlx::query("DELETE FROM genres WHERE id = $1")
            .bind(id)
            .execute(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(id)
    }
}
