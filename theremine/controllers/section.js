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
  router.get('/', isAuthenticated, (req, res) => {
    // console.log('get whishlist/ req.user ' + req.user);
    let wishGroups = [];
    for (let id in req.user.wishGroups) {
      wishGroups.push({id : id, name : req.user.wishGroups[id].name, wishes: req.user.wishGroups[id].wishes});
    }
    res.render('section/section', {
      user: req.user,
      wishGroups: JSON.stringify(wishGroups)
    });
  });

  return router;
};
