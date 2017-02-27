"use strict"
const db = require('../db');

function getGroup(_idUser, groupName) {
  const users = db.get().collection('users');
  users.updateOne(
    { _id: _idUser },
    {
      $push: {
        wishGroups: { name: groupName },
      },
    }
  );
}

function addGroup(_idUser, groupName) {
  const users = db.get().collection('users');
  users.updateOne(
    { _id: _idUser },
    {
      $push: {
        wishGroups: { name: groupName },
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
  let callback = users.updateOne(
    { _id: _idUser },
    {
      $pull : {
        "wishGroups" : null
      }
    }
  );

  users.update(
    { _id: _idUser },
    {
      $unset : {
        [request] : 1
      }
    },
    callback
  );
}

module.exports = {
  add: addGroup,
  remove: removeGroup,
};