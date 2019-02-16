import {Buffer} from 'buffer';
import toBuffer from 'blob-to-buffer';

export default class WsFetch {
  constructor(host, getService) {
    this.host = host;
    this.opened = false;
    this.ws = false;
    this._responseCBS = {};
    this._getService = getService;
  }

  connect(cb) {
    if (this.opened) {
      return cb(true);
    }

    if (this.ws) {
      const wait = () => {
        if (this.opened) {
          return cb(true);
        }
        setTimeout(wait, 200);
      }
      return;
    }

    this.ws = new WebSocket(this.host);
    this.ws.onopen = () => {
      this.opened = true;
      cb(true)
    }
    this.ws.onmessage = ({data}) => {
      toBuffer(data, (err, buf) => {
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
    }

    this.ws.onclose = () => {
      this.opened = false;
      this.ws = false;
    }
  }

  fetch(url, options) {
    options = options || {};
    const re_host = /^https?:\/\/([^\/]+)/i;
    const m =re_host.exec(url);
    options.service = this._getService(m[1]);
    options.pathname = url.replace(m[0], '');
    options.reqid = '' + Math.floor(new Date());
    const req = JSON.stringify(options);

    const h = Buffer.alloc(4);
    h.writeUInt32BE(req.length);
    const buf = Buffer.from(req);
    const data = Buffer.concat([h, buf]);

    return new Promise((resolve, reject) => {
      this.connect((r) => {
        if (!r) {
          return reject(new Error('Connection failed.'));
        }
        this._responseCBS[options.reqid] = (options) => {
          resolve(new Response(options));
        }
        this.ws.send(data);
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
