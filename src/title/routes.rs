use crate::title::{Title, TitleRequest};
use actix_web::{delete, get, post, put, web, HttpResponse, Responder};
use sqlx::PgPool;
use crate::res;

#[get("/titles")]
async fn find_all(db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Title::find_all(db_pool.get_ref()).await;
    match result {
        Ok(xs) => HttpResponse::Ok().json(xs),
        _ => res::r500("Error trying to read all titles from database")
    }
}

#[get("/titles/{id}")]
async fn find(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Title::find_by_id(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => res::r404("Title not found")
    }
}

#[post("/titles")]
async fn create(title: web::Json<TitleRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Title::create(title.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => res::r500("Error trying to create new title")
    }
}

#[put("/titles/{id}")]
async fn update(id: web::Path<i64>, title: web::Json<TitleRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Title::update(id.into_inner(), title.into_inner(),db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => res::r404("Title not found")
    }
}

#[delete("/titles/{id}")]
async fn delete(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = Title::delete(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(rows) => {
            if rows > 0 {
                HttpResponse::Ok().body(format!("Successfully deleted {} record(s)", rows))
            } else {
                res::r404("Title not found")
            }
        },
        _ => res::r404("Title not found")
    }
}

pub fn init(cfg: &mut web::ServiceConfig) {
    cfg.service(find_all);
    cfg.service(find);
    cfg.service(create);
    cfg.service(update);
    cfg.service(delete);
}

