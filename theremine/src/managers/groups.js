const mongo = require('../bs/mongo');
const crypto = require('crypto');

function add(uid, groupName) {
  const users = mongo.db.collection('user');
  const secret = uid;
  const hash = crypto.createHmac('sha256', secret)
    .update(groupName + Date.now().toString())
    .digest('hex');
  users.updateOne(
    { _id: uid },
    {
      $push: {
        wishGroups: {
          id: hash,
          name: groupName,
          wishes: [],
        },
      },
    });
  return hash;
}

function rename(uid, groupId, newName) {
  const users = mongo.db.collection('user');
  const path = 'wishGroups.$.name';
  users.updateOne(
    { 'wishGroups.id': groupId },
    {
      $set: {
        [path]: newName,
      },
    },
  );
}

function remove(uid, groupId) {
  const users = mongo.db.collection('user');
  const path = 'wishGroups.$';
  users.updateOne(
    { 'wishGroups.id': groupId },
    {
      $unset: {
        [path]: 1,
      },
    },
  );
  users.updateOne(
    { _id: uid },
    {
      $pull: {
        wishGroups: null,
      },
    },
  );
}

module.exports = {
  add,
  rename,
  remove,
};
