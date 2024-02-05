FROM debian:buster

ADD mcserver /usr/bin/
ADD mcinit /usr/bin/
ADD run.sh /

VOLUME /var/lib/memcards

WORKDIR /var/lib/memcards

ENV PORT 3846
ENV ROOT_TOKEN TestKey123
ENV BLOCKS 1024

EXPOSE 3846

CMD ["/run.sh"]
