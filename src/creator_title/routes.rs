use crate::creator_title::{CreatorTitle, CreatorTitleRequest};
use actix_web::{delete, get, post, put, web, HttpResponse, Responder};
use sqlx::PgPool;
use crate::res;

#[get("/creator_titles")]
async fn find_all(db_pool: web::Data<PgPool>) -> impl Responder {
    let result = CreatorTitle::find_all(db_pool.get_ref()).await;
    match result {
        Ok(xs) => HttpResponse::Ok().json(xs),
        _ => res::r500("Error trying to read all creator titles from database")
    }
}

#[get("/creator_titles/{id}")]
async fn find(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = CreatorTitle::find_by_id(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => res::r404("Creator title not found")
    }
}

#[post("/creator_titles")]
async fn create(creator_title: web::Json<CreatorTitleRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = CreatorTitle::create(creator_title.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => res::r500("Error trying to create new creator title")
    }
}

#[put("/creator_titles/{id}")]
async fn update(id: web::Path<i64>, creator_title: web::Json<CreatorTitleRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = CreatorTitle::update(id.into_inner(), creator_title.into_inner(),db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => res::r404("Creator title not found")
    }
}

#[delete("/creator_titles/{id}")]
async fn delete(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = CreatorTitle::delete(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(rows) => {
            if rows > 0 {
                HttpResponse::Ok().body(format!("Successfully deleted {} record(s)", rows))
            } else {
                res::r404("Creator title not found")
            }
        },
        _ => res::r404("Creator title not found")
    }
}

pub fn init(cfg: &mut web::ServiceConfig) {
    cfg.service(find_all);
    cfg.service(find);
    cfg.service(create);
    cfg.service(update);
    cfg.service(delete);
}

