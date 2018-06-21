FROM terrorjack/meikyu:ghc-8.2.2

ADD . /usr/src/app
WORKDIR /usr/src/app

RUN apk --no-cache add rocksdb-dev
RUN stack build --no-docker


ENTRYPOINT if [ "$bootnode" = true ] ; then \
             stack exec BootNode-exe; \
           else \
             stack exec SimpleNode-exe; \
           fi
