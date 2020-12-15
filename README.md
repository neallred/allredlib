# Allredlib

Jenna Allred private library

Back end lives in `src/`

Front end lives in `client/`

## Local development
* Install [`rust`](https://www.rust-lang.org), perhaps via [rustup](https://rustup.rs/).
* Install [`sqlx-cli`](https://github.com/launchbadge/sqlx/tree/master/sqlx-cli).
* Install [rust-musl-builder](https://github.com/emk/rust-musl-builder)
* Place `ALLREDLIB_PGUSER`, `ALLREDLIB_PGPASS`, and `ALLREDLIB_DB` in an `.env` file in the repo root.
* Run `docker-compose -f docker-compose-dev-db.yml up`
* Run `sqlx database create` from the repo root.
* Run `docker run --rm -it -v "$(pwd)":/home/rust/src --net=host messense/rust-musl-cross:x86_64-musl cargo build`
* Run `./target/x86_64-unknown-linux-musl/debug/allredlib-server`

## Notes
sqlx does validations while compiling rust code against the running database. It needs to be up in local development for those checks to pass.

