const router = require('express').Router();
var request = require('request');

function isAuthenticated(req, res, next) {
  if (req.isAuthenticated()) {
    next();
    return;
  }
  res.redirect('/');
}

module.exports = function init() {
  // login && default page
  router.get('/search/:search_string', isAuthenticated, (req, res) => {
  	const search_url = 'http://localhost:8081/search?s=' + req.params.search_string;
    res.setHeader('Content-Type', 'application/json');

    var products;
	request(search_url, function (error, response, body) {
		console.log('error:', error);
		console.log('statusCode:', response && response.statusCode);
		// console.log('body:', body);
		products = body;
		// console.log('body:', JSON.parse(products);
		res.send(products);
	});
	// res.send(JSON.stringify(products));
  });

  return router;
};
