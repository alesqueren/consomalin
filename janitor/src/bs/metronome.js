const request = require('request');

const METRONOME_HOST = process.env.METRONOME_HOST || 'http://localhost:3000/tests/attendance.json';

module.exports = {
  send() {
    return new Promise((resolve) => {
      const url = METRONOME_HOST;
      console.log('metronome url : ' + url);
      request(url, (error, response, body) => {
        const slots = body;
        resolve(slots);
      });
    });
  },
};
