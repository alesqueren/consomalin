const router = require('express').Router();

function isAuthenticated(req, res, next) {
  if (req.isAuthenticated()) {
    next();
    return;
  }
  res.redirect('/');
}

module.exports = function init() {
  // login && default page
  router.get('/confirmation', isAuthenticated, (req, res) => {
    res.render('confirmation/confirmation', {});
  });

  return router;
};
