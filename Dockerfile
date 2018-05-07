FROM terrorjack/meikyu:ghc-8.2.2
ENV bootnode false
WORKDIR /usr/src
ADD . Node
RUN cd Node && \
    stack build
EXPOSE 1554 1555 1556 1667
ENTRYPOINT if [ "$bootnode" = true ] ; then \
             stack exec MakeConfigBootNode-exe && \
             stack exec BootNode-exe; \
           else \
             stack exec MakeConfigSimpleNode-exe && \
             stack exec SimpleNode-exe; \
           fi
