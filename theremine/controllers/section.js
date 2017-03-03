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
  router.get('/', isAuthenticated, (req, res) => {
    // console.log('get whishlist/ req.user ' + req.user);
    let wishGroups = [];
    for (let id in req.user.wishGroups) {
      wishGroups.push({id : id, name : req.user.wishGroups[id].name, wishes: req.user.wishGroups[id].wishes});
    }
    res.render('section/section', {
      user: req.user,
      wishGroups: JSON.stringify(wishGroups),
      pcurrentWish: JSON.stringify(req.user.currentWish)||false
    });
  });

  router.put('/section/currentwish/:gid/:wid', isAuthenticated, (req, res) => {
    usersManager.setCurrentWish(req.user._id, req.params.gid, req.params.wid);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });


  return router;
};
