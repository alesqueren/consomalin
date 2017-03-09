"use strict"
const db = require('../db');
const mongo = require('mongodb');
const crypto = require('crypto');
const ObjectID = mongo.ObjectID;

function addWish(_idUser, groupId, wishName) {
  // db.users.update(   {_id : "az@hotmail.fr","wishGroups.id": "58c01b69f2b2143d9432f11c" },   {     $push: {       "wishGroups.$.wishes": { id : "sd", name: 'test'}}});
  const users = db.get().collection('users');
  const secret = _idUser;
  const hash = crypto.createHmac('sha256', secret)
                   .update(wishName+Date.now().toString())
                   .digest('hex');
  let request = "wishGroups.$.wishes";
  users.updateOne(
    { "wishGroups.id": groupId },
    {
      $push: {
        [request]: { id : hash, name: wishName, product: {}}
      },
    },
    (err) => {
      console.log(`err: ${err}`);
    });
  selectWish(_idUser, groupId, hash, true);
}

function selectWish(_idUser, groupId, wishId, selected) {
  var select = selected=='false'?false:true;
  const users = db.get().collection('users');
  const request = "currentBasket.selectedWishes."+groupId+"."+wishId;
  if ( select ) {
    users.updateOne(
      { _id: _idUser },
      {
        $set: {
          [request]: {}
        },
      }
    );
  }else{
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
          "currentBasket.selectedWishes" : null
        }
      }
    );
  }
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
  let request = "currentBasket.selectedWishes."+groupId+"."+wishId+'.product';
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
