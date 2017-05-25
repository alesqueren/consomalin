const mongo = require('../bs/mongo');

const userCollectionName = 'user';

function add(email, password, newsletter, callback) {
  const users = mongo.db.collection(userCollectionName);
  return users.insertOne(
    {
      _id: email,
      password,
      newsletter,
      wishGroups: [],
      currentBasket: {
        currentWishId: null,
        selectedWishes: {},
      },
    },
    (err, cursor) => {
      if (!err) {
        callback(cursor.ops[0]);
      } else {
        callback(false);
      }
    });
}

// TODO: useless ?
function find(email, callback) {
  const users = mongo.db.collection(userCollectionName);
  return users.findOne(
    {
      _id: email,
    },
    (err, user) => {
      callback(user);
    });
}

module.exports = {
  add,
  find,
};
