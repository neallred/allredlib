# Allredlib

Jenna Allred private library

Back end lives in `src/`

Front end lives in `client/`

## Local development
Project related commands all should be run from the repo root.
* Install [`rust`](https://www.rust-lang.org), perhaps via [rustup](https://rustup.rs/).
* Install [`sqlx-cli`](https://github.com/launchbadge/sqlx/tree/master/sqlx-cli).
* Place `ALLREDLIB_PGUSER`, `ALLREDLIB_PGPASS`, and `ALLREDLIB_DB` in an `.env` file in the repo root.
* Run `./x dbup`
* Run `./x dbcreate`
* Run `./x buildfed`
* Run `./x mkserve`
* Run `./x serve`

### Adding to db
* Add a migration with `./x dbmigrate "name of migration"`
* If needed, can run `./x dbreset` to drop and re-add db and run all migrations. Note that app server will need to be stopped or this command will fail.
* Database can be seeded with `./x mkseed && ./x seed` (if db and app are up)

## Prod
Run `./x buildfed && ./x buildserve`. A single, statically linked binary is produced, including all front end assets. If db needs seeding, run `./x buildseed` to create a separate, command line seeder executable.

## Notes
`sqlx` does validations while compiling rust code against the running database. It needs to be up in local development for those checks to pass.
