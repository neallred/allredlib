use serde::{Serialize, Deserialize};
use actix_web::{HttpResponse, HttpRequest, Responder, Error};
use futures::future::{ready, Ready};
use sqlx::{PgPool, FromRow, Row};
use sqlx::postgres::PgRow;
use anyhow::Result;

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct SubseriesAttributionRequest {
    pub subseries_id: i64,
    pub attribution_id: i64,
}

#[derive(Serialize, Deserialize, FromRow, Debug)]
#[serde(rename_all = "camelCase")]
pub struct SubseriesAttribution {
    pub id: i64,
    pub subseries_id: i64,
    pub attribution_id: i64,
}

impl Responder for SubseriesAttribution {
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

impl SubseriesAttribution {
    pub async fn find_all(pool: &PgPool) -> Result<Vec<SubseriesAttribution>> {
        let mut xs = vec![];
        let recs = sqlx::query!(
            r#"
                SELECT id, subseries_id, attribution_id
                    FROM subseries_attributions
                ORDER BY id
            "#
            )
                .fetch_all(pool)
                .await?;

        for r in recs {
            xs.push(SubseriesAttribution {
                id: r.id,
                subseries_id: r.subseries_id,
                attribution_id: r.attribution_id,
            });
        }

        Ok(xs)
    }

    pub async fn find_by_id(id: i64, pool: &PgPool) -> Result<SubseriesAttribution> {
        let r = sqlx::query!(
            r#"
                SELECT * FROM subseries_attributions WHERE id = $1
            "#,
            id
            )
            .fetch_one(&*pool)
            .await?;

        Ok(SubseriesAttribution {
            id: r.id,
            subseries_id: r.subseries_id,
            attribution_id: r.attribution_id,
        })
    }

    pub async fn create(subseries_attribution: SubseriesAttributionRequest, pool: &PgPool) -> Result<SubseriesAttribution> {
        let mut tx = pool.begin().await?;
        let subseries_attribution = sqlx::query("INSERT INTO subseries_attributions (subseries_id, attribution_id) VALUES ($1, $2) RETURNING id, subseries_id, attribution_id")
            .bind(&subseries_attribution.subseries_id)
            .bind(&subseries_attribution.attribution_id)
            .map(|row: PgRow| {
                SubseriesAttribution {
                    id: row.get(0),
                    subseries_id: row.get(1),
                    attribution_id: row.get(2),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(subseries_attribution)
    }

    pub async fn update(id: i64, subseries_attribution: SubseriesAttributionRequest, pool: &PgPool) -> Result<SubseriesAttribution> {
        let mut tx = pool.begin().await.unwrap();
        let subseries_attribution = sqlx::query("UPDATE subseries_attributions SET subseries_id = $2, attribution_id = $3 WHERE id = $1 RETURNING id, subseries_id, attribution_id")
            .bind(id)
            .bind(&subseries_attribution.subseries_id)
            .bind(&subseries_attribution.attribution_id)
            .map(|row: PgRow| {
                SubseriesAttribution {
                    id: row.get(0),
                    subseries_id: row.get(1),
                    attribution_id: row.get(1),
                }
            })
            .fetch_one(&mut tx)
            .await?;

        Ok(subseries_attribution)
    }

    pub async fn delete(id: i64, pool: &PgPool) -> Result<i64> {
        let mut tx = pool.begin().await?;
        sqlx::query("DELETE FROM subseries_attributions WHERE id = $1")
            .bind(id)
            .execute(&mut tx)
            .await?;

        tx.commit().await?;
        Ok(id)
    }
}
