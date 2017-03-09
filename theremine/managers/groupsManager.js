"use strict"
const db = require('../db');
const mongo = require('mongodb');
const crypto = require('crypto');
const ObjectID = mongo.ObjectID;

function addGroup(_idUser, groupName) {
  const users = db.get().collection('users');
  const secret = _idUser;
  const hash = crypto.createHmac('sha256', secret)
                   .update(groupName+Date.now().toString())
                   .digest('hex');
  users.updateOne(
    { _id: _idUser },
    {
      $push: {
        wishGroups: { id : hash, name: groupName, wishes: [] },
      },
    },
    (err) => {
      console.log(`err: ${err}`);
    });
}

function removeGroup(_idUser, groupId) {
  const users = db.get().collection('users');
  let request = "wishGroups."+groupId;
  console.log("req:" + request + _idUser);
  users.updateOne(
    { _id: _idUser },
    {
      $unset : {
        [request] : 1
      }
    }
  );
  users.updateOne(
    { _id: _idUser },
    {
      $pull : {
        "wishGroups" : null
      }
    }
  );
}

module.exports = {
  add: addGroup,
  remove: removeGroup,
};