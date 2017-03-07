"use strict"
const db = require('../db');

function addWish(_idUser, groupId, wishName) {
  const users = db.get().collection('users');
  let request = "wishGroups."+groupId+".wishes";
  users.updateOne(
    { _id: _idUser },
    {
      $push: {
        [request]: {name: wishName, selected: true, product: {}}
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

function renameWish(_idUser, groupId, wishId, newName) {
  const users = db.get().collection('users');
  let request = "wishGroups."+groupId+".wishes."+wishId+".name";
  users.updateOne(
    { _id: _idUser },
    {
      $set: {
        [request]: newName
      },
    }
  );
}

function setProduct(_idUser, groupId, wishId, productId) {
  const users = db.get().collection('users');
  let request = "wishGroups."+groupId+".wishes."+wishId+'.product';
  users.updateOne(
    { _id: _idUser },
    {
      $set: {
        [request]: { 'id' : productId, 'quantity' : 1 }
      },
    }
  );
}
function setProductQty(_idUser, groupId, wishId, qty) {
  const users = db.get().collection('users');
  let request = "wishGroups."+groupId+".wishes."+wishId+".product.quantity";
  users.updateOne(
    { _id: _idUser },
    {
      $set: {
        [request]: qty
      },
    }
  );
}

module.exports = {
  add: addWish,
  select: selectWish,
  rename: renameWish,
  setProduct: setProduct,
  setProductQty: setProductQty,
};
