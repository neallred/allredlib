#! /usr/bin/env bash
# x is for execute

function deps() {
  echo ""
  read -p "This will install rust, sqlx, and docker-compose if they are absent. It will check for but not install docker. Continue? (Y or n) " -n 1 -r
  echo ""
  if [[ ! $REPLY =~ ^[Yy]$ ]]
  then
    exit 1
  fi

  if command -v cargo >/dev/null 2>&1; then
    :
  else
    echo "installing rust"
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    if [ -f $HOME/.cargo/env ]; then
      source $HOME/.cargo/env
    fi
  fi

  if command -v sqlx >/dev/null 2>&1; then
    :
  else
    echo "installing sqlx"
    cargo install sqlx-cli
  fi

  if command -v docker-compose >/dev/null 2>&1; then
    :
  else
    echo "docker not installed. See https://docs.docker.com/engine/install/ for instructions installing."
  fi

  if command -v docker-compose >/dev/null 2>&1; then
    :
  else
    sudo curl -L "https://github.com/docker/compose/releases/download/1.27.4/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
  fi
}

function x() {
  local cmd=$1
  local cmds="buildseed buildserve dbadd dbcreate dbmigrate dbreset dbup deps mkseed mkserve seed serve"
  local tgt_dir="./target/x86_64-unknown-linux-musl/debug"
  case $cmd in
    buildseed)
      cargo build --release --bin allredlib-seeder --features vendored --target x86_64-unknown-linux-musl
      ;;
    buildserve)
      cargo build --release --bin allredlib-server --features vendored --target x86_64-unknown-linux-musl
      ;;
    dbadd)
      sqlx migrate add "${@:2}"
      ;;
    dbcreate)
      sqlx database create
      ;;
    dbmigrate)
      sqlx migrate run
      ;;
    dbreset)
      sqlx database reset
      ;;
    dbup)
      docker-compose -f docker-compose-dev-db.yml up
      ;;
    deps)
      deps
      ;;
    mkseed)
      cargo build --bin allredlib-seeder --features vendored --target x86_64-unknown-linux-musl
      ;;
    mkserve)
      cargo build --bin allredlib-server --features vendored --target x86_64-unknown-linux-musl
      ;;
    seed)
      if [ -f $tgt_dir/allredlib-seeder ]; then
        $tgt_dir/allredlib-seeder
      else
        echo "build the seeder with `./x mkseed`, then rerun this command"
      fi
      ;;
    serve)
      if [ -f $tgt_dir/allredlib-server ]; then
        $tgt_dir/allredlib-server
      else
        echo "build the server with `./x mkserve`, then rerun this command"
      fi
      ;;
    *)
      echo "Can't \"$1\". Try one of: $cmds"
      ;;
  esac
}

x "$@"
