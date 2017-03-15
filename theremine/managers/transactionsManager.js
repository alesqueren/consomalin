"use strict"
const db = require('../db');
const mongo = require('mongodb');
const crypto = require('crypto');
const ObjectID = mongo.ObjectID;
const wishesManager = require('../managers/wishesManager');
const userCollectionName = 'user'

function addTransaction(_idUser, slotId, slotDateTime, wishes) {
  const users = db.get().collection(userCollectionName);
  const secret = _idUser;
  const hash = crypto.createHmac('sha256', secret)
                   .update(slotId+Date.now().toString())
                   .digest('hex');
  let request = "transactions";

  let value = {
    id: hash,
    status: 'transferring',
    value: 10,
    orderTime: Date.now(),
    slot: {
        id: slotId,
        dateTime: slotDateTime
    },
    wishGroups: wishes
  }
  // for(var i = 0; i < wishes.length; i++ ){
  //   var wishGroup = wishes[i];
  //   for(var j = 0; j < wishGroup.wishes.length; j++ ) {
  //     var wish = wishGroup.wishes[j];
  //     wishesManager.select(_idUser, wishGroup.id, wish.id, 'false');
  //   }
  // }
  removeCurrentselectedWishes(_idUser);
  removeCurrentWish(_idUser);
  removeCurrentSlot(_idUser);

  users.updateOne(
    { _id: _idUser },
    {
      $push: {
        [request]: value,
      },
    },
    (err) => {
      console.log(`err: ${err}`);
    });
  return hash;
}

function removeCurrentselectedWishes(_idUser) {
  const users = db.get().collection(userCollectionName);
  let request = "currentBasket.selectedWishes";

  users.updateOne(
    { _id: _idUser },
    {
      $set: {
        [request]: {},
      },
    },
    (err) => {
      console.log(`err: ${err}`);
    });
}

function removeCurrentWish(_idUser) {
  const users = db.get().collection(userCollectionName);
  let request = "currentBasket.currentWish";

  users.updateOne(
    { _id: _idUser },
    {
      $set: {
        [request]: {},
      },
    },
    (err) => {
      console.log(`err: ${err}`);
    });
}

function removeCurrentSlot(_idUser) {
  const users = db.get().collection(userCollectionName);
  let request = "currentBasket.currentSlot";

  users.updateOne(
    { _id: _idUser },
    {
      $set: {
        [request]: {},
      },
    },
    (err) => {
      console.log(`err: ${err}`);
    });
}

module.exports = {
  add: addTransaction,
  removeCurrentselectedWishes: removeCurrentselectedWishes,
  removeCurrentWish: removeCurrentWish,
  removeCurrentSlot: removeCurrentSlot,
};
