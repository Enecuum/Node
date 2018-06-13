FROM terrorjack/meikyu:ghc-8.2.2

ADD . /usr/src/app
WORKDIR /usr/src/app

RUN apk --no-cache add --virtual build-dependencies build-base linux-headers rocksdb-dev && \
    stack build --no-docker && \
    apk del build-dependencies

EXPOSE 1554 1555 1556 1667
ENTRYPOINT if [ "$bootnode" = true ] ; then \
             stack exec BootNode-exe; \
           else \
             stack exec SimpleNode-exe; \
           fi
