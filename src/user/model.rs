use serde::{Serialize, Deserialize};
use sqlx::{PgPool, FromRow, Row};
use sqlx::postgres::PgRow;
use anyhow::{anyhow, Result};
use chrono::{DateTime, Utc};

#[derive(sqlx::Type, Serialize, Deserialize, Debug)]
#[sqlx(rename = "role", rename_all = "lowercase")]
pub enum Role { Anonymous, Unadmitted, User, Admin, SuperAdmin }

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct UserRequest {
    pub username: String,
    pub email: Option<String>,
    pub pass: String, // unencrypted
}

#[derive(Serialize, Deserialize, FromRow, Debug)]
#[serde(rename_all = "camelCase")]
pub struct User {
    pub id: i64,
    pub role: Role,
    pub username: String,
    pub email: Option<String>,
    pub pass: String, // encrypted
}

#[derive(Serialize, Deserialize, FromRow, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Token {
    pub id: i64,
    pub token: String,
    pub user_id: i64,
    pub issued: DateTime<Utc>,
}

impl User {
    pub async fn exists(pool: &PgPool, username: &String) -> Result<bool> {
        let user = sqlx::query!(
            r#"
                SELECT username
                    FROM users
                WHERE username = $1
            "#,
            username
            )
            .fetch_optional(pool)
            .await?;

        Ok(user.is_some())
    }

    pub async fn create(pool: &PgPool, user_request: UserRequest) -> Result<String> {
        let mut tx = pool.begin().await?;
        if User::exists(pool, &user_request.username).await? {
            return Err(anyhow!("Can not create user {} . That user already exists.", user_request.username))
            // return Err(format!("Can not create user {} . That user already exists.", user_request.username))
        }
        // TODO:
        // salt and hash pwd.
        // Generate token.
        let new_user_token = String::from("");
        let salted_and_hashed = String::from(format!("{}", user_request.pass));
        let now = Utc::now();

        let user_id: i64 = sqlx::query("INSERT INTO users (role, username, email, pass) VALUES ($1, $2, $3, $4) RETURNING id")
            .bind(Role::Unadmitted)
            .bind(&user_request.username)
            .bind(&user_request.email)
            .bind(&salted_and_hashed)
            .map(|row: PgRow| row.get(0))
            .fetch_one(&mut tx)
            .await?;

        let _ = sqlx::query("INSERT INTO tokens (token, user_id, issued) VALUES ($1, $2, $3) RETURNING id")
            .bind(&new_user_token)
            .bind(user_id)
            .bind(now)
            .fetch_one(&mut tx)
            .await?;
        tx.commit().await?;

        Ok(new_user_token)
    }

    pub async fn is_logged_in(pool: &PgPool, id: i64, token: String) -> Result<bool> {
        let user_token = sqlx::query!(
            r#"
                SELECT id, token
                    FROM tokens
                WHERE id = $1 AND token = $2
            "#,
            id,
            token
            )
            .fetch_optional(pool)
            .await?;

        Ok(user_token.is_some())
    }
}
