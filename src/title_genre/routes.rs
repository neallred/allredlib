use crate::title_genre::{TitleGenre, TitleGenreRequest};
use actix_web::{delete, get, post, put, web, HttpResponse, Responder};
use sqlx::PgPool;
use crate::res;

#[get("/title_genres")]
async fn find_all(db_pool: web::Data<PgPool>) -> impl Responder {
    let result = TitleGenre::find_all(db_pool.get_ref()).await;
    match result {
        Ok(xs) => HttpResponse::Ok().json(xs),
        _ => res::r500("Error trying to read all title genres from database")
    }
}

#[get("/title_genres/{id}")]
async fn find(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = TitleGenre::find_by_id(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => res::r404("Title Genre not found")
    }
}

#[post("/title_genres")]
async fn create(title_genres: web::Json<TitleGenreRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = TitleGenre::create(title_genres.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => res::r500("Error trying to create new title genre")
    }
}

#[put("/title_genres/{id}")]
async fn update(id: web::Path<i64>, title_genre: web::Json<TitleGenreRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = TitleGenre::update(id.into_inner(), title_genre.into_inner(),db_pool.get_ref()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => res::r404("Title Genre not found")
    }
}

#[delete("/title_genres/{id}")]
async fn delete(id: web::Path<i64>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = TitleGenre::delete(id.into_inner(), db_pool.get_ref()).await;
    match result {
        Ok(rows) => {
            if rows > 0 {
                HttpResponse::Ok().body(format!("Successfully deleted {} record(s)", rows))
            } else {
                res::r404("Title Genre not found")
            }
        },
        _ => res::r404("Title Genre not found")
    }
}

pub fn init(cfg: &mut web::ServiceConfig) {
    cfg.service(find_all);
    cfg.service(find);
    cfg.service(create);
    cfg.service(update);
    cfg.service(delete);
}

