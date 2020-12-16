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
  # TODO: `rust-musl-builder` as currently used downloads the deps on each build, resulting in excessive downloads and slower build times. Find a way to pass in the downloaded deps.

  # from https://github.com/emk/rust-musl-builder
  local build_tag="messense/rust-musl-cross:x86_64-musl"
  local cmd=$1
  local cmds="buildseed buildserve dbadd dbcreate dbmigrate dbreset dbup deps mkseed mkserve seed serve"
  local tgt_dir="./target/x86_64-unknown-linux-musl/debug"
  case $cmd in
    buildseed)
      docker run --rm -it -v "$(pwd)":/home/rust/src --net=host $build_tag cargo build --release --bin allredlib-server
      ;;
    buildserve)
      docker run --rm -it -v "$(pwd)":/home/rust/src --net=host $build_tag cargo build --release --bin allredlib-server
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
      docker run --rm -it -v "$(pwd)":/home/rust/src --net=host $build_tag cargo build --bin allredlib-seeder
      ;;
    mkserve)
      docker run --rm -it -v "$(pwd)":/home/rust/src --net=host $build_tag cargo build --bin allredlib-server
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
