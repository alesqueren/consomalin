const router = require('express').Router();
const groupsManager = require('../managers/groupsManager');

function isAuthenticated(req, res, next) {
  if (req.isAuthenticated()) {
    next();
    return;
  }
  res.redirect('/');
}

module.exports = function init() {
  router.get('/', isAuthenticated, (req, res) => {
    // console.log('get whishlist/ req.user ' + req.user);
    res.render('wishList/wishList', { user: req.user });
  });

  router.post('/groups', isAuthenticated, (req, res) => {
    groupsManager.add(req.user._id, req.body.name);
    // console.log(req.user);
    console.log(`req.user._id ${req.user._id}`);
    console.log(`req.body.name ${req.body.name}`);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });

  return router;
};
