const request = require('request');

const RECORDER_HOST = process.env.RECORDER_HOST;

module.exports = {
  send(params, sid, reqbody) {
    return new Promise((resolve) => {
      const url = RECORDER_HOST + 'history/sessions/' + sid + '/' + encodeURI(params);
      // console.log('recorder url : ' + url);
      request.post(url, { form: reqbody }, (error, response, body) => {
        resolve(body);
      });
    });
  },
};
