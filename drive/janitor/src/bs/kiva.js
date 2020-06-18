const request = require('request');

const KIVA_HOST = process.env.KIVA_HOST || 'http://localhost:8083/';

module.exports = {
  send(params) {
    return new Promise((resolve) => {
      const url = KIVA_HOST + encodeURI(params);
      request.debug = true;
      request(url, (error, response, body) => {
        resolve(body);
      });
      request.debug = false;
    });
  },
};
