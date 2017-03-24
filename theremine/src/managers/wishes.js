const mongo = require('../bs/mongo');
const crypto = require('crypto');

function selectWish(idUser, groupId, wishId, selected) {
  const select = selected;
  const users = mongo.db.collection('user');
  const request = `currentBasket.selectedWishes.${groupId}.${wishId}`;
  if (select) {
    users.updateOne(
      { _id: idUser },
      {
        $set: {
          [request]: {},
        },
      },
    );
  } else {
    users.updateOne(
      { _id: idUser },
      {
        $unset: {
          [request]: 1,
        },
      },
    );
    users.updateOne(
      { _id: idUser },
      {
        $pull: {
          'currentBasket.selectedWishes': null,
        },
      },
    );
  }
}

function addWish(idUser, groupId, wishName) {
  const users = mongo.db.collection('user');
  const secret = idUser;
  const hash = crypto.createHmac('sha256', secret)
    .update(wishName + Date.now().toString())
    .digest('hex');
  const request = 'wishGroups.$.wishes';
  users.updateOne(
    { 'wishGroups.id': groupId },
    {
      $push: {
        [request]: {
          id: hash,
          name: wishName,
        },
      },
    },
    (err) => {
      console.log(`err: ${err}`);
    },
  );
  selectWish(idUser, groupId, hash, true);
  return hash;
}

function renameWish(idUser, groupId, wishId, newName) {
  // TODO: merge with removeWish
  const users = mongo.db.collection('user');
  users.findOne({ _id: idUser }, (err, document) => {
    const wishGroups = document.wishGroups;
    for (let i = 0; i < wishGroups.length; i++) {
      const wishGroup = wishGroups[i];
      const wishGroupLength = wishGroup.wishes ? wishGroup.wishes.length : 0;
      for (let j = 0; j < wishGroupLength; j++) {
        const wish = wishGroup.wishes[j];
        if (wishGroup.id === groupId && wish.id === wishId) {
          wish.name = newName;
        }
      }
    }
    users.updateOne(
      { _id: idUser },
      {
        $set: { wishGroups },
      },
    );
  });
}

function removeWish(idUser, groupId, wishId) {
  // il faut aussi supprimer le wish des selectedWish si il y est.
  const users = mongo.db.collection('user');
  users.findOne({ _id: idUser },
    (err, document) => {
      const wishGroups = document.wishGroups;
      for (let i = 0; i < wishGroups.length; i++) {
        const wishGroup = wishGroups[i];
        const wishGroupLength = wishGroup.wishes ? wishGroup.wishes.length : 0;
        for (let j = 0; j < wishGroupLength; j++) {
          const wish = wishGroup.wishes[j];
          if (wishGroup.id === groupId && wish.id === wishId) {
            wishGroup.wishes.splice(j, 1);
            break;
          }
        }
      }
      users.updateOne(
        { _id: idUser },
        {
          $set: { wishGroups },
        },
      );
    },
  );
}

function setProduct(idUser, groupId, wishId, productId) {
  const users = mongo.db.collection('user');
  const request = `currentBasket.selectedWishes.${groupId}.${wishId}.product`;
  users.updateOne(
    { _id: idUser },
    {
      $set: {
        [request]: {
          id: productId,
          quantity: 1,
        },
      },
    },
  );
}

function setProductQty(idUser, groupId, wishId, qty) {
  const users = mongo.db.collection('user');
  const request = `currentBasket.selectedWishes.${groupId}.${wishId}.product.quantity`;
  users.updateOne(
    { _id: idUser },
    {
      $set: {
        [request]: parseInt(qty, 10),
      },
    },
  );
}

module.exports = {
  add: addWish,
  select: selectWish,
  rename: renameWish,
  remove: removeWish,
  setProduct,
  setProductQty,
};
