use serde::{Serialize, Deserialize};
use actix_web::{HttpResponse, HttpRequest, Responder, Error};
use futures::future::{ready, Ready};
use sqlx::{PgPool, FromRow, Row};
use sqlx::postgres::PgRow;
use anyhow::Result;

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct TitleGenreRequest {
    pub title_id: i64,
    pub genre_id: i64,
}

#[derive(Serialize, Deserialize, FromRow, Debug)]
#[serde(rename_all = "camelCase")]
pub struct TitleGenre {
    pub id: i64,
    pub title_id: i64,
    pub genre_id: i64,
}

impl Responder for TitleGenre {
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

impl TitleGenre {
    pub async fn find_all(pool: &PgPool) -> Result<Vec<TitleGenre>> {
        let mut xs = vec![];
        let recs = sqlx::query!(
            r#"
                SELECT id, title_id, genre_id
                    FROM title_genres
                ORDER BY id
            "#
            )
                .fetch_all(pool)
                .await?;

        for r in recs {
            xs.push(TitleGenre {
                id: r.id,
                title_id: r.title_id,
                genre_id: r.genre_id,
            });
        }

        Ok(xs)
    }

    pub async fn find_by_id(id: i64, pool: &PgPool) -> Result<TitleGenre> {
        let r = sqlx::query!(
            r#"
                SELECT * FROM title_genres WHERE id = $1
            "#,
            id
            )
            .fetch_one(&*pool)
            .await?;

        Ok(TitleGenre {
            id: r.id,
            title_id: r.title_id,
            genre_id: r.genre_id,
        })
    }

    pub async fn create(title_genre: TitleGenreRequest, pool: &PgPool) -> Result<TitleGenre> {
        let mut tx = pool.begin().await?;
        let title_genre = sqlx::query("INSERT INTO title_genres (title_id, genre_id) VALUES ($1, $2) RETURNING id, title_id, genre_id")
            .bind(title_genre.title_id)
            .bind(title_genre.genre_id)
            .map(|row: PgRow| {
                TitleGenre {
                    id: row.get(0),
                    title_id: row.get(1),
                    genre_id: row.get(2),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(title_genre)
    }

    pub async fn update(id: i64, title_genre: TitleGenreRequest, pool: &PgPool) -> Result<TitleGenre> {
        let mut tx = pool.begin().await.unwrap();
        let title_genre = sqlx::query("UPDATE title_genres SET title_id = $2, genre_id = $3 WHERE id = $1 RETURNING id, title_id, genre_id")
            .bind(id)
            .bind(title_genre.title_id)
            .bind(title_genre.genre_id)
            .map(|row: PgRow| {
                TitleGenre {
                    id: row.get(0),
                    title_id: row.get(1),
                    genre_id: row.get(2),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        Ok(title_genre)
    }

    pub async fn delete(id: i64, pool: &PgPool) -> Result<i64> {
        let mut tx = pool.begin().await?;
        sqlx::query("DELETE FROM title_genres WHERE id = $1")
            .bind(id)
            .execute(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(id)
    }
}
