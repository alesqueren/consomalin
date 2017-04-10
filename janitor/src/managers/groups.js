const mongo = require('../bs/mongo');
const utils = require('../utils');

function add(uid, gname) {
  const users = mongo.db.collection('user');
  const hash = utils.randHash(uid, gname);
  users.updateOne(
    { _id: uid },
    {
      $push: {
        wishGroups: {
          id: hash,
          name: gname,
          wishes: [],
        },
      },
    });
  return hash;
}

function rename(uid, gid, newName) {
  const users = mongo.db.collection('user');
  const path = 'wishGroups.$.name';
  users.updateOne(
    { 'wishGroups.id': gid },
    {
      $set: {
        [path]: newName,
      },
    },
  );
}

function select(uid, wishGroups, gid) {
  const selectWishes = {};
  for (const i in wishGroups) {
    const group = wishGroups[i];
    if (group.id === gid) {
      for (const j in group.wishes) {
        const wish = group.wishes[j];
        selectWishes[wish.id] = {};
      }
    }
  }

  const users = mongo.db.collection('user');
  const path = `currentBasket.selectedWishes.${gid}`;
  users.update(
    { _id: uid },
    {
      $set: {
        [path]: selectWishes,
      },
    },
  );
}

function unselect(uid, gid) {
  const users = mongo.db.collection('user');
  const path = `currentBasket.selectedWishes.${gid}`;
  users.update(
    { _id: uid },
    {
      $unset: {
        [path]: '',
      },
    },
  );
}

function remove(uid, gid) {
  const users = mongo.db.collection('user');
  users.updateOne(
    { _id: uid },
    {
      $pull: {
        wishGroups: {
          id: gid,
        },
      },
    },
    { multi: true });
}

module.exports = {
  add,
  rename,
  select,
  unselect,
  remove,
};
