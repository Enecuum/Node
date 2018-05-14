FROM fpco/stack-run
ENV bootnode false
ADD . /usr/src/Node
WORKDIR /usr/src/Node

RUN stack --stack-yaml=CI.stack.yaml build

EXPOSE 1554 1555 1556 1667

ENTRYPOINT if [ "$bootnode" = true ] ; then \
             stack exec MakeConfigBootNode-exe && \
             stack exec BootNode-exe; \
           else \
             stack exec MakeConfigSimpleNode-exe && \
             stack exec SimpleNode-exe; \
           fi
