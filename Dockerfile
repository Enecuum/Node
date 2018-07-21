FROM terrorjack/meikyu:ghc-8.2.2 as builder
ADD . /usr/src/app
WORKDIR /usr/src/app
RUN apk --no-cache add build-base linux-headers rocksdb-dev
RUN stack build && mkdir bin && stack install --local-bin-path /usr/src/app/bin

FROM alpine:latest
ENV bootnode false
WORKDIR /usr/src/app
COPY --from=builder /usr/src/app/bin /usr/src/app
COPY --from=builder /usr/src/app/configs /usr/src/app/configs
RUN echo "http://dl-cdn.alpinelinux.org/alpine/edge/main" >> /etc/apk/repositories; \
    echo "http://dl-cdn.alpinelinux.org/alpine/edge/community" >> /etc/apk/repositories; \
    echo "http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
RUN apk --update add rocksdb-dev
ENTRYPOINT if [ "$bootnode" = true ]; then echo "Bootnode activated" && ./BootNode-exe; else echo "Node activated" && ./SimpleNode-exe; fi
