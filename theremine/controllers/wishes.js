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

  //create wishes
  router.post('/wishlist/groups/:gid/wishes/bulk', isAuthenticated, (req, res) => {
    let groupId = req.params.gid;
    for(var i in req.body.names) {
      wishesManager.add(req.user._id, groupId, req.body.names[i]);
    }
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });

  //select a wish
  router.put('/wishlist/groups/:gid/wishes/:wid', isAuthenticated, (req, res) => {
    let groupId = req.params.gid;
    let wishId = req.params.wid;
    wishesManager.select(req.user._id, groupId, wishId, req.body.selected);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });

  //rename a wish
  router.put('/wishlist/groups/:gid/wishes/:wid/rename', isAuthenticated, (req, res) => {
    wishesManager.rename(req.user._id, req.body.name);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });

  //set a product
  router.post('/wishlist/groups/:gid/wishes/:wid/product', isAuthenticated, (req, res) => {
    let groupId = req.params.gid;
    let wishId = req.params.wid;
    wishesManager.setProduct(req.user._id, groupId, wishId, req.body.pid);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });


  return router;
};
