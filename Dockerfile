# мы берем базовый образ terrorjack/meikyu (этот чувак собрал образ на
# альпине и поставил туда свежий stack и ghc + регулярно контрибьютит,
# когда как у fpco больше года никакой активности),
FROM terrorjack/meikyu:ghc-8.2.2

# копируем папку с конфигами
COPY ./configs ./configs

ADD . /usr/src/app
WORKDIR /usr/src/app

# apk - это менеджер пакетов на alpine.
RUN apk --no-cache add rocksdb-dev

# stack build компилирует внутри контейнера и выдает бинарник...
RUN stack build --no-docker

# ...который мы и запускаем.  Однако пока я отключил его автоматический
# запуск, чтобы иметь возможность заходить в контейнер консолью и
# смотреть содержимое
# ENTRYPOINT if [ "$bootnode" = true ] ; then \
#              stack exec BootNode-exe; \
#            else \
#              stack exec SimpleNode-exe; \
#            fi
