"use strict"
const mongo = require('../bs/mongo');
const crypto = require('crypto');

function addWish(_idUser, groupId, wishName) {
  // mongo.users.update(   {_id : "az@hotmail.fr","wishGroups.id": "58c01b69f2b2143d9432f11c" },   {     $push: {       "wishGroups.$.wishes": { id : "sd", name: 'test'}}});
  const users = mongo.get().collection('user');
  const secret = _idUser;
  const hash = crypto.createHmac('sha256', secret)
                   .update(wishName+Date.now().toString())
                   .digest('hex');
  let request = "wishGroups.$.wishes";
  users.updateOne(
    { "wishGroups.id": groupId },
    {
      $push: {
        [request]: { id : hash, name: wishName}
      },
    },
    (err) => {
      console.log(`err: ${err}`);
    });
  selectWish(_idUser, groupId, hash, true);
}

function selectWish(_idUser, groupId, wishId, selected) {
  var select = selected=='false'?false:true;
  const users = mongo.get().collection('user');
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
  const users = mongo.get().collection('user');
  users.findOne({_id: _idUser}, function(err, document) {
    var wishGroups = document.wishGroups;
    for(var i = 0; i < wishGroups.length; i++ ) {
        var wishGroup = wishGroups[i];
        var wishGroupLength = wishGroup.wishes?wishGroup.wishes.length:0;
        for(var j = 0; j < wishGroupLength; j++ ) {
            var wish = wishGroup.wishes[j];
            if( wishGroup.id == groupId && wish.id == wishId ) {
                wish.name = newName;
            }
        }
    }
    users.updateOne(
      { _id: _idUser },
      {
        $set: {
          "wishGroups": wishGroups
        },
      }
    );
  });
}

function removeWish(_idUser, groupId, wishId) {
  const users = mongo.get().collection('user');
  users.findOne({_id: _idUser}, function(err, document) {
    var wishGroups = document.wishGroups;
    for(var i = 0; i < wishGroups.length; i++ ) {
        var wishGroup = wishGroups[i];
        var wishGroupLength = wishGroup.wishes?wishGroup.wishes.length:0;
        for(var j = 0; j < wishGroupLength; j++ ) {
            var wish = wishGroup.wishes[j];
            if( wishGroup.id == groupId && wish.id == wishId ) {
                wishGroup.wishes.splice(j, 1);
                break;
            }
        }
    }
    users.updateOne(
      { _id: _idUser },
      {
        $set: {
          "wishGroups": wishGroups
        },
      }
    );
  });
}

function setProduct(_idUser, groupId, wishId, productId) {
  const users = mongo.get().collection('user');
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
  const users = mongo.get().collection('user');
  let request = "currentBasket.selectedWishes."+groupId+"."+wishId+".product.quantity";
  users.updateOne(
    { _id: _idUser },
    {
      $set: {
        [request]: parseInt(qty, 10)
      },
    }
  );
}

module.exports = {
  add: addWish,
  select: selectWish,
  rename: renameWish,
  remove: removeWish,
  setProduct: setProduct,
  setProductQty: setProductQty,
};
