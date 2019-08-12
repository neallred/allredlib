# BUILD STATIC FILE SERVER
FROM debian:buster as build
WORKDIR /build-mount
RUN apt-get update && apt-get install -y \
  curl \
  git \
  nodejs \
  && \
  curl https://get.haskellstack.org/ | sh && \
  rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/neallred/allredlib.git
WORKDIR allredlib
RUN git pull && apt-get remove -y git
# build backend before static assets. They are less likely to change
RUN stack build --copy-bins --local-bin-path ./ && rm -rf ~/.stack && rm -rf .stack-work

# STATIC FILE BUILD
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.34.0/install.sh | bash && \
  [ -s "~/.nvm/nvm.sh" ] && \. "~/.nvm/nvm.sh" && \
  nvm install 10 && \
  npm i && \
  npm run build

# ASSEMBLE FINAL IMAGE
FROM debian:buster
RUN mkdir /server-mount
COPY --from=build /build-mount/allredlib/server /server-mount/server
COPY --from=build /build-mount/allredlib/static /server-mount/static
WORKDIR server-mount/


# RUN STATIC SERVER
CMD ["./server"]
