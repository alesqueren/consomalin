"use strict"

const router = require('express').Router();
const groupsManager = require('../managers/groupsManager');
const wishesManager = require('../managers/wishesManager');

function isAuthenticated(req, res, next) {
  if (req.isAuthenticated()) {
    next();
    return;
  }
  res.redirect('/');
}

module.exports = function init() {
  router.get('/wishlist', isAuthenticated, (req, res) => {
    var selectedWishes = req.user.currentBasket?req.user.currentBasket.selectedWishes:{};
    res.render('wishlist/wishlist', {
      user: req.user,
      wishGroups: JSON.stringify(req.user.wishGroups),
      selectedWishes: JSON.stringify(selectedWishes)
    });
  });

  return router;
};
