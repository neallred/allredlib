use crate::subseries::{Subseries, SubseriesRequest};
use actix_web::{delete, get, post, put, web, HttpResponse, Responder};
use sqlx::PgPool;

fn r400(x: &'static str) -> HttpResponse {
    HttpResponse::BadRequest().body(x)
}

#[get("/subseries")]
async fn find_all(db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Subseries::find_all(db_pool.get_ref()).await;
    match result {
        Ok(xs) => HttpResponse::Ok().json(xs),
        _ => r400("Error trying to read all subseries from database")
    }
}

#[get("/subseries/{id}")]
async fn find(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Subseries::find_by_id(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => r400("Subseries not found")
    }
}

#[post("/subseries")]
async fn create(subseries: web::Json<SubseriesRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Subseries::create(subseries.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => r400("Error trying to create new subseries")
    }
}

#[put("/subseries/{id}")]
async fn update(id: web::Path<i64>, subseries: web::Json<SubseriesRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Subseries::update(id.into_inner(), subseries.into_inner(),db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => r400("Subseries not found")
    }
}

#[delete("/subseries/{id}")]
async fn delete(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Subseries::delete(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(rows) => {
            if rows > 0 {
                HttpResponse::Ok().body(format!("Successfully deleted {} record(s)", rows))
            } else {
                r400("Subseries not found")
            }
        },
        _ => r400("Subseries not found")
    }
}

pub fn init(cfg: &mut web::ServiceConfig) {
    cfg.service(find_all);
    cfg.service(find);
    cfg.service(create);
    cfg.service(update);
    cfg.service(delete);
}
