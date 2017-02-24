"use strict"
const db = require('../db');

function addWish(_idUser, groupId, wishName) {
  const users = db.get().collection('users');
  let request = "wishGroups["+groupId+"].wishes";
  users.updateOne(
    { _id: _idUser },
    {
      $push: {
        request: {name: wishName}
      },
    },
    (err) => {
      console.log(`err: ${err}`);
    });
}

module.exports = {
  add: addWish,
};
