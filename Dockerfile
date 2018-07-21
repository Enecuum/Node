FROM terrorjack/meikyu:ghc-8.2.2 as builder
ADD . /usr/src/app
WORKDIR /usr/src/app
RUN apk --no-cache add build-base linux-headers rocksdb-dev
RUN stack build && mkdir bin && stack install --local-bin-path /usr/src/app/bin

FROM terrorjack/meikyu:ghc-8.2.2
ENV bootnode false
COPY --from=builder /usr/src/app/bin /usr/src/app
COPY --from=builder /usr/src/app/configs /usr/src/app/configs
RUN apk --no-cache add rocksdb-dev
ENTRYPOINT if [ "$bootnode" = true ]; then echo "Bootnode activated" && ./BootNode-exe; else echo "Node activated" && ./SimpleNode-exe; fi
