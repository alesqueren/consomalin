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
  router.push('/rename', isAuthenticated, (req, res) => {
    wishesManager.rename(req.user._id, req.body.name);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });


  return router;
};
