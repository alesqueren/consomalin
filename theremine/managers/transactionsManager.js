"use strict"
const db = require('../db');
const mongo = require('mongodb');
const crypto = require('crypto');
const ObjectID = mongo.ObjectID;
const wishesManager = require('../managers/wishesManager');

function addTransaction(_idUser, slotId, slotDateTime, wishes) {
  const users = db.get().collection('users');
  const secret = _idUser;
  const hash = crypto.createHmac('sha256', secret)
                   .update(slotId+Date.now().toString())
                   .digest('hex');
  let request = "transactions."+hash;

  let value = {
    status: 'transferring',
    value: '10',
    orderTime: Date.now(),
    slot: {
        id: slotId,
        dateTime: slotDateTime
    },
    wishGroups: wishes
  }
  for(var i = 0; i < wishes.length; i++ ){
    var wishGroup = wishes[i];
    for(var j = 0; j < wishGroup.wishes.length; j++ ) {
      var wish = wishGroup.wishes[j];
      wishesManager.select(_idUser, wishGroup.id, wish.id, 'false');
    }
  }

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

module.exports = {
  add: addTransaction,
};