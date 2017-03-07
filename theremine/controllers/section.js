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

function getCurrentWishEntity( user ){
  var currentWish;
  const noCurrentWish = !user.currentWish;
  for(var i = 0; i < user.wishGroups.length; i++ ) {
    var wishGroup = user.wishGroups[i];
    var wishGroupLength = wishGroup.wishes?wishGroup.wishes.length:0;
    for(var j = 0; j < wishGroupLength; j++ ) {
      var wish = wishGroup.wishes[j];
      if( wish.selected ) {
          var currentWishDefinedAndGoodIndexes = user.currentWish && user.currentWish.group == i && user.currentWish.wish == j;
          if ( noCurrentWish || currentWishDefinedAndGoodIndexes ) {
              currentWish = wish;
              currentWish.groupId = wishGroup.id;
              currentWish.groupName = wishGroup.name;
              currentWish.id = j;
              return currentWish;
          }
      }
    }
  }
  return null;
}

module.exports = function init() {
  router.get('/', isAuthenticated, (req, res) => {
    // console.log('get whishlist/ req.user ' + req.user);
    let wishGroups = [];
    for (let id in req.user.wishGroups) {
      wishGroups.push({id : id, name : req.user.wishGroups[id].name, wishes: req.user.wishGroups[id].wishes});
    }
    const currentwish = getCurrentWishEntity( req.user );
    res.render('section/section', {
      user: req.user,
      wishGroups: JSON.stringify(wishGroups),
      currentWish: JSON.stringify(currentwish)
    });
  });

  router.put('/currentwish/:gid/:wid', isAuthenticated, (req, res) => {
    usersManager.setCurrentWish(req.user._id, req.params.gid, req.params.wid);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));
  });
  return router;
};
