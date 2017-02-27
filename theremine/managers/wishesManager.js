"use strict"
const db = require('../db');

function addWish(_idUser, groupId, wishName) {
  const users = db.get().collection('users');
  let request = "wishGroups."+groupId+".wishes";
  users.updateOne(
    { _id: _idUser },
    {
      $push: {
        [request]: {name: wishName, selected: true}
      },
    },
    (err) => {
      console.log(`err: ${err}`);
    });
}

function selectWish(_idUser, groupId, wishId, selected) {
  var select = selected=='false'?false:true;
  const users = db.get().collection('users');
  let request = "wishGroups."+groupId+".wishes."+wishId+".selected";
  users.updateOne(
    { _id: _idUser },
    {
      $set: {
        [request]: select
      },
    }
  );
}

module.exports = {
  add: addWish,
  select: selectWish,
};
