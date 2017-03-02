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
    res.render('wishlist/wishlist', {
      user: req.user,
      wishGroups: JSON.stringify(wishGroups)
    });
  });

  router.post('/groups', isAuthenticated, (req, res) => {
    groupsManager.add(req.user._id, req.body.name);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });

  router.delete('/groups/:gid', (req, res) => {
    let groupId = req.params.gid;
    groupsManager.remove("az@hotmail.fr", groupId);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });

  router.post('/groups/:gid/wishes/bulk', isAuthenticated, (req, res) => {
    let groupId = req.params.gid;
    for(var i in req.body.names) {
      wishesManager.add(req.user._id, groupId, req.body.names[i]);
    }
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });

  router.put('/groups/:gid/wishes/:wid', isAuthenticated, (req, res) => {
    let groupId = req.params.gid;
    let wishId = req.params.wid;
    wishesManager.select(req.user._id, groupId, wishId, req.body.selected);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });

  router.put('/groups/:gid', isAuthenticated, (req, res) => {
    let groupId = req.params.gid;
    for(let id in req.user.wishGroups[groupId]) {
      wishesManager.select(req.user._id, groupId, req.user.wishGroups[groupId][id], req.body.selected);
    }
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });


  return router;
};
