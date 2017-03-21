const mongo = require('../bs/mongo');
const userCollectionName = 'user'

function addUser(email, password, callback) {
  const users = mongo.get().collection(userCollectionName);
  return users.insertOne(
    {
      _id: email,
      password
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
  const users = mongo.get().collection(userCollectionName);
  return users.findOne(
    {
      _id: email,
    },
    (err, user) => {
      // console.log(user);
      callback(user);
    });
}

function setCurrentWish(email, groupId, wishId) {
  const users = mongo.get().collection(userCollectionName);
  const request = "currentBasket.currentWish";
  users.updateOne(
    { _id: email },
    {
      $set: {
        [request]: { 'group' : groupId, 'wish' : wishId }
      },
    }
  );
}

function removeCurrentWish(email, groupId, wishId) {
  const users = mongo.get().collection(userCollectionName);
  const request = "currentBasket.currentWish";
  users.updateOne(
    { _id: email },
    {
      $set: {
        [request]: {}
      },
    }
  );
}

function setCurrentSlot(email, slot) {
  const users = mongo.get().collection(userCollectionName);
  const request = "currentBasket.currentSlot";
  users.updateOne(
    { _id: email },
    {
      $set: {
        [request]: slot
      },
    }
  );
}

module.exports = {
  add: addUser,
  find: findUser,
  setCurrentWish: setCurrentWish,
  removeCurrentWish: removeCurrentWish,
  setCurrentSlot: setCurrentSlot,
};
