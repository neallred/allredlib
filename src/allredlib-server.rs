use actix_web::{web, App, HttpRequest, HttpServer, Responder};
use sqlx::postgres::{PgPoolOptions, Postgres};
use sqlx::migrate::{MigrateDatabase};
use anyhow::Result;
use env_logger::Env;

use std::env;
use dotenv::dotenv;

mod creator;
mod series;
mod subseries_attribution;
mod subseries;
mod attribution;
mod res;

async fn hi(req: HttpRequest) -> impl Responder {
    let name = req.match_info().get("name").unwrap_or("World");
    format!("Hello {}!\n", &name)
}

#[actix_web::main]
async fn main() -> Result<()> {
    dotenv().ok();

    env_logger::Builder::from_env(Env::default().default_filter_or("info")).init();

    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL not in .env file");
    let host = env::var("HOST").expect("HOST not in .env file");
    let port = env::var("PORT").expect("PORT not in .env file");

    let db_exists: bool = Postgres::database_exists(&database_url).await?;
    if !db_exists {
        println!("db duz not exist");
        Postgres::create_database(&database_url).await?;
    }

    let db_pool = PgPoolOptions::new()
        .max_connections(5)
        .connect(&database_url).await?;

    HttpServer::new(move || {
        App::new()
            .data(db_pool.clone())
            .route("/", web::get().to(hi))
            .configure(creator::init)
            .configure(series::init)
            .configure(attribution::init)
            .configure(subseries::init)
            .configure(subseries_attribution::init)
    })
    .bind(format!("{}:{}", host, port))?
    .run()
    .await?;
    Ok(())
}
