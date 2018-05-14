FROM terrorjack/meikyu:ghc-8.2.2
ENV bootnode false
#ADD . /usr/src/Node
WORKDIR /usr/src/Node

RUN apk --no-cache add --virtual build-dependencies git
RUN git clone git@github.com:Enecuum/Node.git .
RUN apk del build-dependencies
RUN stack --stack-yaml=CI.stack.yaml build

EXPOSE 1554 1555 1556 1667

ENTRYPOINT if [ "$bootnode" = true ] ; then \
             stack exec MakeConfigBootNode-exe && \
             stack exec BootNode-exe; \
           else \
             stack exec MakeConfigSimpleNode-exe && \
             stack exec SimpleNode-exe; \
           fi
