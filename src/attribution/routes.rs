use crate::attribution::{Attribution, AttributionRequest};
use actix_web::{delete, get, post, put, web, HttpResponse, Responder};
use sqlx::PgPool;

fn r400(x: &'static str) -> HttpResponse {
    HttpResponse::BadRequest().body(x)
}

#[get("/attributions")]
async fn find_all(db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Attribution::find_all(db_pool.get_ref()).await;
    match result {
        Ok(xs) => HttpResponse::Ok().json(xs),
        _ => r400("Error trying to read all attributions from database")
    }
}

#[get("/attributions/{id}")]
async fn find(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Attribution::find_by_id(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => r400("Attribution not found")
    }
}

#[post("/attributions")]
async fn create(attribution: web::Json<AttributionRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Attribution::create(attribution.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => r400("Error trying to create new attribution")
    }
}

#[put("/attributions/{id}")]
async fn update(id: web::Path<i64>, attribution: web::Json<AttributionRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Attribution::update(id.into_inner(), attribution.into_inner(),db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => r400("Attribution not found")
    }
}

#[delete("/attributions/{id}")]
async fn delete(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Attribution::delete(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(rows) => {
            if rows > 0 {
                HttpResponse::Ok().body(format!("Successfully deleted {} record(s)", rows))
            } else {
                r400("Attribution not found")
            }
        },
        _ => r400("Attribution not found")
    }
}

pub fn init(cfg: &mut web::ServiceConfig) {
    cfg.service(find_all);
    cfg.service(find);
    cfg.service(create);
    cfg.service(update);
    cfg.service(delete);
}
