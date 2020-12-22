use crate::genre::{Genre, GenreRequest};
use actix_web::{delete, get, post, put, web, HttpResponse, Responder};
use sqlx::PgPool;
use crate::res;

#[get("/genres")]
async fn find_all(db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Genre::find_all(db_pool.get_ref()).await;
    match result {
        Ok(xs) => HttpResponse::Ok().json(xs),
        _ => res::r500("Error trying to read all genres from database")
    }
}

#[get("/genres/{id}")]
async fn find(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Genre::find_by_id(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => res::r404("Genre not found")
    }
}

#[post("/genres")]
async fn create(title: web::Json<GenreRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Genre::create(title.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => res::r500("Error trying to create new title")
    }
}

#[put("/genres/{id}")]
async fn update(id: web::Path<i64>, title: web::Json<GenreRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Genre::update(id.into_inner(), title.into_inner(),db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => res::r404("Genre not found")
    }
}

#[delete("/genres/{id}")]
async fn delete(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Genre::delete(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(rows) => {
            if rows > 0 {
                HttpResponse::Ok().body(format!("Successfully deleted {} record(s)", rows))
            } else {
                res::r404("Genre not found")
            }
        },
        _ => res::r404("Genre not found")
    }
}

pub fn init(cfg: &mut web::ServiceConfig) {
    cfg.service(find_all);
    cfg.service(find);
    cfg.service(create);
    cfg.service(update);
    cfg.service(delete);
}
