"use strict"
const db = require('../db');
const mongo = require('mongodb');
const crypto = require('crypto');
const ObjectID = mongo.ObjectID;

function addGroup(_idUser, groupName) {
  const users = db.get().collection('user');
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
    });
  return hash;
}

function renameGroup(_idUser, groupId, newName) {
  const users = db.get().collection('user');
  let request = "wishGroups."+groupId+".name";
  users.updateOne(
    { _id: _idUser },
    {
      $set : {
        [request] : newName
      }
    }
  );
}

function removeGroup(_idUser, groupId) {
  const users = db.get().collection('user');
  let request = "wishGroups."+groupId;
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