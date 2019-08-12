# BUILD STATIC FILE SERVER
FROM debian:buster as build
WORKDIR /build-mount
RUN apt-get update && apt-get install -y \
  curl \
  git \
  && \
  curl https://get.haskellstack.org/ | sh && \
  apt-get remove curl \
  rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/neallred/allredlib.git
WORKDIR allredlib
RUN git pull && apt-get remove git
# build backend before static assets. They are less likely to change
RUN stack build --copy-bins --local-bin-path ./ && rm -rf ~/.stack && rm -rf .stack-work

# STATIC FILE BUILD
RUN npx pulp build --main Main --include client --to static/index.js && cp client/index.html static/index.html

# ASSEMBLE FINAL IMAGE
FROM debian:buster
RUN mkdir /server-mount
COPY --from=build /build-mount/allredlib/server /server-mount/server
COPY --from=build /build-mount/allredlib/static /server-mount/static
WORKDIR server-mount/

# RUN STATIC SERVER
CMD ["./server"]
