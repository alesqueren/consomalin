const request = require('request');

const RECORDER_HOST = process.env.RECORDER_HOST;

module.exports = {
  // TODO: factorise
  sendGet(sid) {
    return new Promise((resolve) => {
      const url = RECORDER_HOST + 'sessions/' + sid;
      request.get(url, (error, response, body) => {
        resolve(body);
      });
    });
  },
  sendPost(params, sid, reqbody) {
    return new Promise((resolve) => {
      const url = RECORDER_HOST + 'sessions/' + sid + '/' + encodeURI(params);
      const opts = {
        url,
        method: 'POST',
        json: true,
        body: reqbody,
      };
      request(opts, (error, response, body) => {
        resolve(body);
      });
    });
  },
};
