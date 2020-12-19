use actix_web::{HttpResponse};
use log::{warn, error};

pub fn r404(x: &'static str) -> HttpResponse {
    HttpResponse::NotFound().body(x)
}
pub fn r500(x: &'static str) -> HttpResponse {
    error!("{}", x);
    HttpResponse::BadRequest().body(x)
}

