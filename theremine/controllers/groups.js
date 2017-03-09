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

  //create a group
  router.post('/wishlist/groups', isAuthenticated, (req, res) => {
    const groupId = groupsManager.add(req.user._id, req.body.name);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify(groupId));
  });

  //delete a group
  router.delete('/wishlist/groups/:gid', (req, res) => {
    let groupId = req.params.gid;
    groupsManager.remove("az@hotmail.fr", groupId);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });

  //select a group
  router.put('/wishlist/groups/:gid', isAuthenticated, (req, res) => {
    let groupId = req.params.gid;
    for(let id in req.user.wishGroups[groupId]) {
      wishesManager.select(req.user._id, groupId, req.user.wishGroups[groupId][id], req.body.selected);
    }
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });


  return router;
};
