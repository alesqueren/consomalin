const request = require('request');

const WENDY_HOST = process.env.WENDY_HOST || 'http://localhost:8087/';

module.exports = {
  send(partUrl, data) {
    return new Promise((resolve) => {
      const url = WENDY_HOST + encodeURI(partUrl);
      request.debug = true;
      const jsonData = JSON.stringify(data);
      request.post(url, { form: jsonData }, (error, response, body) => {
        const statusCode = response && response.statusCode;
        resolve({ statusCode, result: body });
      });
      request.debug = false;
    });
  },
};
