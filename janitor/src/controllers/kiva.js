const router = require('express').Router();
const mid = require('../middlewares');
const request = require('request');

const KIVA_HOST = process.env.KIVA_HOST || 'localhost:8083';

router.get('/:kiva_uri*',
  mid.isAuthenticated,
  ({ params, data, user }, res) => {
    const url = KIVA_HOST + encodeURI(params.kiva_uri);
    request(url, (error, response, body) => {
      response = body;
      res.json(response);
    });
  },
);

module.exports = function init() {
  return router;
};
