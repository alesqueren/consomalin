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
    res.render('wishList/wishList', {
      user: req.user,
      wishGroups: JSON.stringify(wishGroups)
    });
  });

  router.post('/groups', isAuthenticated, (req, res) => {
    groupsManager.add(req.user._id, req.body.name);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });

  router.post('/groups/:gid/wishes/bulk', isAuthenticated, (req, res) => {
    let groupId = request.params.gid;
    for(var i in req.body.names){
      wishesManager.add(req.user._id, groupId, req.body.names);
    }
    console.log(`req.user._id ${req.user._id}`);
    console.log(`req.body.names ${req.body.names}`);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });

  return router;
};
