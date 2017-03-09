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

  //create wishes
  router.post('/wishlist/groups/:gid/wishes/bulk', isAuthenticated, (req, res) => {
    let groupId = req.params.gid;
    //renvoi le dernier hash utilisé car pour le moment on ne crée pas plus d'un wish à la fois
    for(var i in req.body.names) {
      var hash = wishesManager.add(req.user._id, groupId, req.body.names[i]);
    }
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify(hash));
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
    let groupId = req.params.gid;
    let wishId = req.params.wid;
    wishesManager.rename(req.user._id, groupId, wishId, req.body.name);
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

  //set a product quantity
  router.put('/wishlist/groups/:gid/wishes/:wid/product', isAuthenticated, (req, res) => {
    let groupId = req.params.gid;
    let wishId = req.params.wid;
    wishesManager.setProductQty(req.user._id, groupId, wishId, req.body.qty);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });

  //set current wish
  router.put('/wishlist/groups/:gid/wishes/:wid/current', isAuthenticated, (req, res) => {
    usersManager.setCurrentWish(req.user._id, req.params.gid, req.params.wid);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });

  return router;
};
