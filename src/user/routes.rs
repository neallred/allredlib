use crate::user::{User, UserRequest};
use actix_web::{delete, get, post, put, web, HttpResponse, Responder};
use sqlx::PgPool;
use crate::res;

#[post("/users")]
async fn create(user: web::Json<UserRequest>, db_pool: web::Data<PgPool>) -> impl Responder {
    let result = User::create(db_pool.get_ref(), user.into_inner()).await;
    match result {
        Ok(x) => HttpResponse::Ok().json(x),
        _ => res::r500("Error trying to create new user")
    }
}


pub fn init(cfg: &mut web::ServiceConfig) {
    cfg.service(create);
}
