const db = require('../db');
const userCollectionName = 'user'

function addUser(email, password, callback) {
  const users = db.get().collection(userCollectionName);
  return users.insertOne(
    {
      _id: email,
      password
    },
    (err, user) => {
      console.log('user contents', JSON.stringify(user, null, 4));
      console.log('user inserted', JSON.stringify(user.ops[0], null, 4));
      // console.log(userCollectionName);
      // console.log(user[0]);
      callback(user.ops[0]);
    });
}

function findUser(email, callback) {
  const users = db.get().collection(userCollectionName);
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
  const users = db.get().collection(userCollectionName);
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
  const users = db.get().collection(userCollectionName);
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
  const users = db.get().collection(userCollectionName);
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
