const mongo = require('../bs/mongo');

const userCollectionName = 'user';

function addUser(email, password, callback) {
  const users = mongo.db.collection(userCollectionName);
  return users.insertOne(
    {
      _id: email,
      password,
    },
    (err, cursor) => {
      if (!err) {
        callback(cursor.ops[0]);
      } else {
        callback(false);
      }
    });
}

function findUser(email, callback) {
  const users = mongo.db.collection(userCollectionName);
  return users.findOne(
    {
      _id: email,
    },
    (err, user) => {
      callback(user);
    });
}

function setCurrentWish(email, groupId, wishId) {
  const users = mongo.db.collection(userCollectionName);
  const path = 'currentBasket.currentWish';
  users.updateOne(
    { _id: email },
    {
      $set: {
        [path]: {
          group: groupId,
          wish: wishId,
        },
      },
    },
  );
}

function removeCurrentWish(email) {
  const users = mongo.db.collection(userCollectionName);
  const path = 'currentBasket.currentWish';
  users.updateOne(
    { _id: email },
    {
      $set: {
        [path]: {},
      },
    },
  );
}

function setCurrentSlot(email, slot) {
  const users = mongo.db.collection(userCollectionName);
  const path = 'currentBasket.currentSlot';
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
  add: addUser,
  find: findUser,
  setCurrentWish,
  removeCurrentWish,
  setCurrentSlot,
};
