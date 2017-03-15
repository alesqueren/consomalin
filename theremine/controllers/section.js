"use strict"

const router = require('express').Router();
const groupsManager = require('../managers/groupsManager');
const wishesManager = require('../managers/wishesManager');
const usersManager = require('../managers/usersManager');

function isAuthenticated(req, res, next) {
  if (req.isAuthenticated()) {
    next();
    return;
  }
  res.redirect('/');
}

module.exports = function init() {
  router.get('/section', isAuthenticated, (req, res) => {
    // console.log('get whishlist/ req.user ' + req.user);
    var wishGroups = req.user.wishGroups?req.user.wishGroups:{};
    var selectedWishes = req.user.currentBasket?req.user.currentBasket.selectedWishes:{};
    var pCurrentWish = req.user.currentBasket?req.user.currentBasket.currentWish?req.user.currentBasket.currentWish:'false':'false';
    res.render('section/section', {
      user: req.user,
      wishGroups: JSON.stringify(wishGroups),
      pCurrentWish: JSON.stringify(pCurrentWish),
      pSelectedWishes: JSON.stringify(selectedWishes)
    });
  });
  return router;
};
