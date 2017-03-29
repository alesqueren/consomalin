const router = require('express').Router();
const mid = require('../middlewares');
const utils = require('../utils');
const request = require('request');

const KIVA_HOST = process.env.KIVA_HOST || 'localhost:8083';

router.get('/:kiva_uri*',
  mid.isAuthenticated,
  ({ params, data, user }, res) => {
    console.log('req.params.kiva_uri : ' + params.kiva_uri);
    const url = 'http://' + KIVA_HOST + '/' + params.kiva_uri;
    console.log('url : ' + url);
    request(url, (error, response, body) => {
      response = body;
      res.json(response);
    });
  },
);

module.exports = function init() {
  return router;
};
