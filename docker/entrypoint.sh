#!/bin/bash

set -o errexit

configfile="/config.yml"

if [ ! -f "${configfile}" ]; then
  touch ${configfile}
  for (( i = 1; ; i++ ))
  do
    VAR_SERVICE="SERVICE$i"
    VAR_HOST="HOST$i"

    if [ ! -n "${!VAR_SERVICE}" ]; then
      break
    fi

    it_service=${!VAR_SERVICE}
    it_host=${!VAR_HOST}

    cat >> ${configfile} <<_EOF_
- service: ${it_service}
  host: ${it_host}
_EOF_
  done
fi

cat ${configfile}

if [ "$1" = 'ws-fetch' ]; then
  exec /usr/bin/ws-fetch ${configfile}
fi

exec "$@"
