use serde::{Serialize, Deserialize};
use actix_web::{HttpResponse, HttpRequest, Responder, Error};
use futures::future::{ready, Ready};
use sqlx::{PgPool, FromRow, Row};
use sqlx::postgres::PgRow;
use anyhow::Result;

#[derive(Serialize, Deserialize, Debug)]
pub struct TitleRequest {
    pub title: String,
    pub year: Option<i32>,
    pub synopsis: Option<String>,
    pub seriesPart: Option<String>,
    pub seriesId: Option<String>,
    pub subseriesPart: Option<String>,
    pub subseriesId: Option<String>,
}

#[derive(Serialize, Deserialize, FromRow, Debug)]
pub struct Title {
    pub id: i64,
}

