const db = require('../db');

function addUser(email, password, callback) {
  const users = db.get().collection('users');
  return users.insertOne(
    {
      _id: email,
      password
    },
    (err, user) => {
      console.log('user contents', JSON.stringify(user, null, 4));
      console.log('user inserted', JSON.stringify(user.ops[0], null, 4));
      // console.log('user');
      // console.log(user[0]);
      callback(user.ops[0]);
    });
}

function findUser(email, callback) {
  const users = db.get().collection('users');
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
  var select = selected=='false'?false:true;
  const users = db.get().collection('users');
  const request = "currentWish";
  users.updateOne(
    { _id: _idUser },
    {
      $set: {
        [request]: { group : groupId, wish : wishId }
      },
    }
  );
}

module.exports = {
  add: addUser,
  find: findUser,
};
