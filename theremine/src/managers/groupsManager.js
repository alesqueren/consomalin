"use strict"
const mongo = require('../bs/mongo');
const crypto = require('crypto');

function addGroup(_idUser, groupName) {
  const users = mongo.get().collection('user');
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
  const users = mongo.get().collection('user');
  let request = "wishGroups.$.name";
  users.updateOne(
    { "wishGroups.id": groupId },
    {
      $set : {
        [request] : newName
      }
    }
  );
}

function removeGroup(_idUser, groupId) {
  const users = mongo.get().collection('user');
  console.log('want delete');
  console.log(groupId);
  let request = "wishGroups.$";
  users.updateOne(
    { "wishGroups.id": groupId },
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
