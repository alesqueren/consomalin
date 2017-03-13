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
  // for(var i = 0; i < wishes.length; i++ ){
  //   var wish = wishes[i];
  //   wishesManager.select(_idUser, wish.groupId, wish.id, 'false');
  // }

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