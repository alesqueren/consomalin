const mongo = require('../bs/mongo');

const userCollectionName = 'user';

function setCurrentWish(email, wid) {
  const users = mongo.db.collection(userCollectionName);
  const path = 'currentBasket.currentWishId';
  users.updateOne(
    { _id: email },
    {
      $set: {
        [path]: wid,
      },
    },
  );
}
function removeCurrentWish(email) {
  const users = mongo.db.collection(userCollectionName);
  const path = 'currentBasket.currentWishId';
  users.updateOne(
    { _id: email },
    {
      $set: {
        [path]: null,
      },
    },
  );
}
function removeCurrentselectedWishes(email) {
  const users = mongo.db.collection(userCollectionName);
  const path = 'currentBasket.selectedWishes';
  users.updateOne(
    { _id: email },
    {
      $set: {
        [path]: null,
      },
    },
  );
}
function removeCurrentSlot(email) {
  const users = mongo.db.collection(userCollectionName);
  const path = 'currentBasket.slot';
  users.updateOne(
    { _id: email },
    {
      $set: {
        [path]: null,
      },
    },
  );
}
function setCurrentSlot(email, slot) {
  const users = mongo.db.collection(userCollectionName);
  const path = 'currentBasket.slot';
  users.updateOne(
    { _id: email },
    {
      $set: {
        [path]: slot,
      },
    },
  );
}

module.exports = {
  setCurrentWish,
  removeCurrentWish,
  removeCurrentselectedWishes,
  removeCurrentSlot,
  setCurrentSlot,
};
