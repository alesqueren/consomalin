const router = require('express').Router();

module.exports = function init() {
  // login && default page
  router.get('/', (req, res) => {
    res.render('index', { message: req.flash('message') });
  });

  return router;
};
