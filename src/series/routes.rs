use crate::series::{Series, SeriesRequest};
use actix_web::{delete, get, post, put, web, HttpResponse, Responder};
use sqlx::PgPool;

fn r400(x: &'static str) -> HttpResponse {
    HttpResponse::BadRequest().body(x)
}

#[get("/series")]
async fn find_all(db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Series::find_all(db_pool.get_ref()).await;
    match result {
        Ok(xs) => HttpResponse::Ok().json(xs),
        _ => r400("Error trying to read all series from database")
    }
}

#[get("/series/{id}")]
async fn find(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Series::find_by_id(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => r400("Series not found")
    }
}

#[post("/series")]
async fn create(series: web::Json<SeriesRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Series::create(series.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => r400("Error trying to create new series")
    }
}

#[put("/series/{id}")]
async fn update(id: web::Path<i64>, series: web::Json<SeriesRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Series::update(id.into_inner(), series.into_inner(),db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => r400("Series not found")
    }
}

#[delete("/series/{id}")]
async fn delete(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Series::delete(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(rows) => {
            if rows > 0 {
                HttpResponse::Ok().body(format!("Successfully deleted {} record(s)", rows))
            } else {
                r400("Series not found")
            }
        },
        _ => r400("Series not found")
    }
}

pub fn init(cfg: &mut web::ServiceConfig) {
    cfg.service(find_all);
    cfg.service(find);
    cfg.service(create);
    cfg.service(update);
    cfg.service(delete);
}
