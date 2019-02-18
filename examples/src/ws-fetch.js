import {Buffer} from 'buffer';
import toBuffer from 'blob-to-buffer';
import fetch from 'isomorphic-fetch';
import uuid from 'uuid';

export default class WsFetch {
  constructor(host) {
    this.host = host;
    this.opened = false;
    this.ws = false;
    this._responseCBS = {};
    this.debug = false;
    this.disabled = false;
  }

  connect(cb) {
    if (this.opened) {
      return cb(true);
    }

    if (this.ws) {
      return cb(false);
    }

    this.ws = new WebSocket(this.host);
    this.ws.onopen = () => {
      this.opened = true;
    };
    this.ws.onmessage = ({data, ...opts}) => {
      if (data instanceof Blob) {
        toBuffer(data, (err, buf) => {
          if (this.debug) {
            console.log('receive:', buf.toString());
          }
          const h = buf[0];
          const options = JSON.parse(buf.slice(1, h + 1).toString());
          options.body = buf.slice(h+1, buf.length);
          const f = this._responseCBS[options.resid];
          if (f) {
            f(options);
          } else {
            console.log(options);
          }
        });
      } else {
        const options = JSON.parse(data);
        if (this.debug) {
          console.log('receive:', options);
        }
        const f = this._responseCBS[options.resid];
        if (f) {
          f(options);
        } else {
          console.log(options);
        }
      }
    };

    this.ws.onclose = () => {
      this.opened = false;
      this.ws = false;
    };
    return cb(false);
  }

  fetch(service, url, options) {
    if (this.disabled) {
      return fetch(url, options);
    }
    return new Promise((resolve, reject) => {
      this.connect((r) => {
        if (!r) {
          return resolve(fetch(url, options));
        }

        const {method, headers, body} = options || {};

        if (body instanceof FormData) {
          return resolve(fetch(url, options));
        }


        let isText = true;
        if (body && typeof body !== 'string') {
          isText = false;
        }

        const re_host = /^https?:\/\/([^/]+)/i;
        const m =re_host.exec(url);
        const pathname = url.replace(m[0], '');
        const reqid = '' + uuid.v4();

        const extra = isText ? {body} : {};

        let req = JSON.stringify({headers, method, service, pathname, reqid, ...extra});

        if (!isText) {
          const h = Buffer.alloc(4);
          h.writeUInt32BE(req.length);
          req = Buffer.concat([h, Buffer.from(req), Buffer.from(body || '')]);
        }

        if (this.debug) {
          console.log('send:', req.toString());
        }

        this._responseCBS[reqid] = (options) => {
          delete this._responseCBS[reqid];
          resolve(new Response(options));
        };

        try {
          this.ws.send(req);
        } catch (e) {
          delete this._responseCBS[reqid];
          resolve(fetch(url, options));
        }
      });
    });
  }
}

class Response {
  constructor(options) {
    this.options = options;
    this.headers = new Headers(options);
  }
  async json() {
    return JSON.parse(this.options.body.toString());
  }
  async text() {
    return this.options.body.toString();
  }
}

class Headers {
  constructor(options) {
    this.options = options;
  }
  get(n) {
    if (n.toLowerCase() === 'content-type') {
      return this.options.contentType;
    }
    return '';
  }
}
