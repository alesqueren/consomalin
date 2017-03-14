const router = require('express').Router();
const request = require('request');
const KIVA_HOST = process.env.KIVA_HOST || 'http://localhost:8081';

function isAuthenticated(req, res, next) {
  if (req.isAuthenticated()) {
    next();
    return;
  }
  res.redirect('/');
}

module.exports = function init() {
  router.get('/products/search/:search_string', isAuthenticated, (req, res) => {
    const search_url = KIVA_HOST + '/search?s=' + req.params.search_string;
    res.setHeader('Content-Type', 'application/json');

    var products;
    request(search_url, function (error, response, body) {
      // console.log('error:', error);
      // console.log('statusCode:', response && response.statusCode);
      // console.log('body:', body);
      products = body;
      // console.log('body:', JSON.parse(products);
      res.send(products);
    });
  // res.send(JSON.stringify(products));
  });

  // http://localhost:8081/details?pids=[%22271293%22]
  router.get('/products/details', isAuthenticated, (req, res) => {
    const details_url = KIVA_HOST + '/details?pids='+req.query.pids;
    res.setHeader('Content-Type', 'application/json');

    var products;
    request(details_url, function (error, response, body) {
      // console.log('error:', error);
      // console.log('statusCode:', response && response.statusCode);
      // console.log('body:', body);
      products = body;
      // console.log('body:', JSON.parse(products);
      res.send(products);
    });
  // res.send(JSON.stringify(products));
  });

  return router;
};
