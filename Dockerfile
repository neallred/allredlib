# BUILD STATIC FILE SERVER
FROM debian:buster as build
WORKDIR /build-mount
RUN apt-get update && apt-get install -y \
  curl \
  git \
  libncurses5 \
  nodejs \
  && \
  curl https://get.haskellstack.org/ | sh && \
  rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/neallred/allredlib.git
WORKDIR allredlib

RUN stack build --copy-bins --local-bin-path ./ && rm -rf ~/.stack && rm -rf .stack-work

# STATIC FILE BUILD
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.34.0/install.sh | bash && \
  [ -s "/root/.nvm/nvm.sh" ] && \. "/root/.nvm/nvm.sh" && \
  nvm install 10 && \
  npm i && \
  npx psc-package install && \
  npm run build

# ASSEMBLE FINAL IMAGE
FROM debian:buster
RUN mkdir /server-mount
COPY --from=build /build-mount/allredlib/server /server-mount/server
COPY --from=build /build-mount/allredlib/static /server-mount/static
WORKDIR server-mount/

# RUN STATIC SERVER
CMD ["./server"]
