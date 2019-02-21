FROM alpine:3.8
ENV VERSION v0.1.1.0
RUN apk update && \
    apk add wget bash && \
    wget https://github.com/Lupino/ws-fetch/releases/download/$VERSION/ws-fetch-linux-$VERSION.gz && \
    apk del wget && \
    gunzip ws-fetch-linux-$VERSION.gz && \
    mv ws-fetch-linux-$VERSION /usr/bin/ws-fetch && \
    chmod +x /usr/bin/ws-fetch

COPY entrypoint.sh /entrypoint.sh

EXPOSE 3000

ENTRYPOINT ["/entrypoint.sh"]

CMD ["ws-fetch"]
