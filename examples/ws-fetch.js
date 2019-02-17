import {Buffer} from 'buffer';
import toBuffer from 'blob-to-buffer';

export default class WsFetch {
  constructor(host) {
    this.host = host;
    this.opened = false;
    this.ws = false;
    this._responseCBS = {};
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
    }
    this.ws.onmessage = ({data}) => {
      toBuffer(data, (err, buf) => {
        const h = buf[0];
        const options = JSON.parse(buf.slice(1, h + 1).toString());
        options.body = buf.slice(h+1, buf.length);
        const f = this._responseCBS[options.resid];
        if (f) {
          delete this._responseCBS[options.resid];
          f(options);
        } else {
          console.log(options);
        }
      });
    }

    this.ws.onclose = () => {
      this.opened = false;
      this.ws = false;
    }
    return cb(false);
  }

  fetch(url, options) {
    return new Promise((resolve, reject) => {
      this.connect((r) => {
        if (!r) {
          return resolve(fetch(url, options));
        }

        options = options || {};
        const re_host = /^https?:\/\/([^/]+)/i;
        const m =re_host.exec(url);
        const service = m[1];
        const pathname = url.replace(m[0], '');
        const reqid = '' + Math.floor(new Date());
        const req = JSON.stringify({...options, service, pathname, reqid});

        const h = Buffer.alloc(4);
        h.writeUInt32BE(req.length);
        const buf = Buffer.from(req);
        const data = Buffer.concat([h, buf]);
        this._responseCBS[reqid] = (options) => {
          delete this._responseCBS[reqid];
          resolve(new Response(options));
        }
        try {
          this.ws.send(data);
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
    for (const k in this.options) {
      if (k.lowerCase() === n.lowerCase()) {
        return this.options[k];
      }
    }
    return '';
  }
}
