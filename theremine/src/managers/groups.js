const mongo = require('../bs/mongo');
const crypto = require('crypto');

function addGroup(idUser, groupName) {
  const users = mongo.db.collection('user');
  const secret = idUser;
  const hash = crypto.createHmac('sha256', secret)
    .update(groupName + Date.now().toString())
    .digest('hex');
  users.updateOne(
    { _id: idUser },
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

function renameGroup(idUser, groupId, newName) {
  const users = mongo.db.collection('user');
  const request = 'wishGroups.$.name';
  users.updateOne(
    { 'wishGroups.id': groupId },
    {
      $set: {
        [request]: newName,
      },
    },
  );
}

function removeGroup(idUser, groupId) {
  const users = mongo.db.collection('user');
  const request = 'wishGroups.$';
  users.updateOne(
    { 'wishGroups.id': groupId },
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
        wishGroups: null,
      },
    },
  );
}

module.exports = {
  add: addGroup,
  rename: renameGroup,
  remove: removeGroup,
};
