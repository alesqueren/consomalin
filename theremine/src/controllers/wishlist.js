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
    
    // console.log(req.user);
    // res.send("done");

    var currentBasket = req.user.currentBasket?req.user.currentBasket:{};
    res.send(JSON.stringify({
      wishGroups: req.user.wishGroups,
      currentBasket: currentBasket
    }));

    // res.render('wishlist/wishlist', {
    //   user: req.user,
    //   wishGroups: JSON.stringify(req.user.wishGroups),
    //   selectedWishes: JSON.stringify(selectedWishes)
    // });
  });

  return router;
};
