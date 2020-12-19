use crate::subseries_attribution::{SubseriesAttribution, SubseriesAttributionRequest};
use actix_web::{delete, get, post, put, web, HttpResponse, Responder};
use sqlx::PgPool;
use crate::res;

#[get("/subseries_attributions")]
async fn find_all(db_pool: web::Data<PgPool>) -> impl Responder {
    let result = SubseriesAttribution::find_all(db_pool.get_ref()).await;
    match result {
        Ok(xs) => HttpResponse::Ok().json(xs),
        _ => res::r500("Error trying to read all subseries attributions from database")
    }
}

#[get("/subseries_attributions/{id}")]
async fn find(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = SubseriesAttribution::find_by_id(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => res::r404("Subseries not found")
    }
}

#[post("/subseries_attributions")]
async fn create(subseries_attribution: web::Json<SubseriesAttributionRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = SubseriesAttribution::create(subseries_attribution.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => res::r500("Error trying to create new subseries attribution")
    }
}

#[put("/subseries_attributions/{id}")]
async fn update(id: web::Path<i64>, subseries_attribution: web::Json<SubseriesAttributionRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = SubseriesAttribution::update(id.into_inner(), subseries_attribution.into_inner(),db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => res::r404("Subseries attribution not found")
    }
}

#[delete("/subseries_attributions/{id}")]
async fn delete(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = SubseriesAttribution::delete(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(rows) => {
            if rows > 0 {
                HttpResponse::Ok().body(format!("Successfully deleted {} record(s)", rows))
            } else {
                res::r404("Subseries attribution not found")
            }
        },
        _ => res::r404("Subseries attribution not found")
    }
}

pub fn init(cfg: &mut web::ServiceConfig) {
    cfg.service(find_all);
    cfg.service(find);
    cfg.service(create);
    cfg.service(update);
    cfg.service(delete);
}
