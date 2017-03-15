"use strict"

const router = require('express').Router();
const request = require('request');
const groupsManager = require('../managers/groupsManager');
const wishesManager = require('../managers/wishesManager');
const usersManager = require('../managers/usersManager');
const transactionsManager = require('../managers/transactionsManager');
const METRONOME_HOST = process.env.METRONOME_HOST || 'http://localhost:3000/tests/attendance.json';
const rabbitMQ = require('../rabbitMQ');
const KIVA_HOST = process.env.KIVA_HOST || 'http://localhost:8081';

function isAuthenticated(req, res, next) {
  if (req.isAuthenticated()) {
    next();
    return;
  }
  res.redirect('/');
}

module.exports = function init() {
  router.get('/withdraw', isAuthenticated, (req, res) => {
    // console.log('get whishlist/ req.user ' + req.user);
    var wishGroups = req.user.wishGroups?req.user.wishGroups:{};
    var selectedWishes = req.user.currentBasket?req.user.currentBasket.selectedWishes:{};
    res.render('withdraw/withdraw', {
      user: req.user,
      wishGroups: JSON.stringify(wishGroups),
      pCurrentWish: JSON.stringify(req.user.currentWish||null),
      pSelectedWishes: JSON.stringify(selectedWishes)
    });
  });

  //recuperer les slots
  router.get('/withdraw/search', isAuthenticated, (req, res) => {
    const search_url = METRONOME_HOST;
    res.setHeader('Content-Type', 'application/json');

    var slots;
    request(search_url, function (error, response, body) {
      // console.log('statusCode:', response && response.statusCode);
      slots = body;
      res.send(slots);
    });
  });

  //save the slot
  router.post('/withdraw/select', isAuthenticated, (req, res) => {
    const slot_id = req.body.slot_id;
    const slot_dateTime = req.body.slot_dateTime;
    // transactionsManager.getLastTransaction(req.user._id)
    usersManager.setCurrentSlot(req.user._id, {slot_id: slot_id, slot_dateTime: slot_dateTime});

    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify('OK'));


  });

  //Passer la commande
  router.post('/withdraw/confirm', isAuthenticated, (req, res) => {
    const slot_id = req.user.currentBasket.currentSlot.slot_id;
    const slot_dateTime = req.user.currentBasket.currentSlot.slot_dateTime;
    // console.log('slotid : ' + slot_id)
    // console.log('slot_dateTime : ' + slot_dateTime)


    var wishGroups = req.user.wishGroups;
    var pSelectedWishes = req.user.currentBasket.selectedWishes;


    var productsToDetail = [];
    var wishGroupsToSave = [];
    var selectedWishes = [];
    for(var i = 0; i < wishGroups.length; i++ ) {
        var wishGroup = wishGroups[i];
        var wishGroupLength = wishGroup.wishes?wishGroup.wishes.length:0;
        var wishesToInsert = [];
        for(var j = 0; j < wishGroupLength; j++ ) {
            var wish = wishGroup.wishes[j];
            var product = pSelectedWishes[wishGroup.id]?pSelectedWishes[wishGroup.id][wish.id]?pSelectedWishes[wishGroup.id][wish.id].product:false:false;
            if( product ) {
              // console.log('product' )
              // console.log(product )
                var wishToInsert = {
                  name: wish.name, 
                  product: {id: product.id}, 
                  quantity: parseInt(product.quantity, 10)
                }
                wishesToInsert.push(wishToInsert);
                productsToDetail.push(product.id);
            }
        }
        if ( wishesToInsert.length )
          wishGroupsToSave.push({ name: wishGroup.name, wishes : wishesToInsert});
    }

    const details_url = KIVA_HOST + '/details?pids='+JSON.stringify(productsToDetail);
    // console.log('details_url : ' + details_url);
    var products;
    request(details_url, function (error, response, body) {
      // console.log('error:', error);
      // console.log('statusCode:', response && response.statusCode);
      products = JSON.parse(body);
      // console.log('body:', products);
      for(var i = 0; i < wishGroupsToSave.length; i++ ) {
          var wishGroup = wishGroupsToSave[i];
          var wishGroupLength = wishGroup.wishes?wishGroup.wishes.length:0;
          var wishesToInsert = [];
          for(var j = 0; j < wishGroupLength; j++ ) {
              var wish = wishGroup.wishes[j];
              var product = products[wish.product.id];
              wish.product.image = product.imageUrl
          }
      }
      var transaction_id = transactionsManager.add(req.user._id, slot_id, slot_dateTime, wishGroupsToSave);
      const data = {
         "user": req.user._id,
         "transaction": transaction_id
      }
      //comment when dev
      // rabbitMQ.send(JSON.stringify(data), null);

      res.setHeader('Content-Type', 'application/json');
      res.send(JSON.stringify('OK'));
    });


  });

  return router;
};
