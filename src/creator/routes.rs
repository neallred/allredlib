use crate::creator::{Creator, CreatorRequest};
use actix_web::{delete, get, post, put, web, HttpResponse, Responder};
use sqlx::PgPool;

fn r400(x: &'static str) -> HttpResponse {
    HttpResponse::BadRequest().body(x)
}

#[get("/creators")]
async fn find_all(db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Creator::find_all(db_pool.get_ref()).await;
    match result {
        Ok(creators) => HttpResponse::Ok().json(creators),
        _ => r400("Error trying to read all creators from database")
    }
}

#[get("/creators/{id}")]
async fn find(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Creator::find_by_id(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(creator) => HttpResponse::Ok().json(creator),
        _ => r400("Creator not found")
    }
}

#[post("/creator")]
async fn create(creator: web::Json<CreatorRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Creator::create(creator.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(creator) => HttpResponse::Ok().json(creator),
        _ => r400("Error trying to create new creator")
    }
}

#[put("/creator/{id}")]
async fn update(id: web::Path<i64>, creator: web::Json<CreatorRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Creator::update(id.into_inner(), creator.into_inner(),db_pool.get_ref()).await;
    match result {
        Ok(creator) => HttpResponse::Ok().json(creator),
        _ => HttpResponse::BadRequest().body("Creator not found")
    }
}

#[delete("/creator/{id}")]
async fn delete(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Creator::delete(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(rows) => {
            if rows > 0 {
                HttpResponse::Ok().body(format!("Successfully deleted {} record(s)", rows))
            } else {
                r400("Creator not found")
            }
        },
        _ => r400("Creator not found")
    }
}

pub fn init(cfg: &mut web::ServiceConfig) {
    cfg.service(find_all);
    cfg.service(find);
    cfg.service(create);
    cfg.service(update);
    cfg.service(delete);
}
