const mongo = require('../bs/mongo');
const utils = require('../utils');

function select(uid, gid, wid, selected) {
  const users = mongo.db.collection('user');
  const path = `currentBasket.selectedWishes.${gid}.${wid}`;
  if (selected) {
    users.updateOne(
      { _id: uid },
      {
        $set: {
          [path]: {},
        },
      },
    );
  } else {
    users.updateOne(
      { _id: uid },
      {
        $unset: {
          [path]: '',
        },
      },
    );
  }
}

function add(uid, gid, wname, selected) {
  const users = mongo.db.collection('user');
  const hash = utils.randHash(uid, wname);
  const path = 'wishGroups.$.wishes';
  users.updateOne(
    { 'wishGroups.id': gid },
    {
      $push: {
        [path]: {
          id: hash,
          name: wname,
        },
      },
    },
  );
  if (selected !== false) {
    console.log('selected');
    select(uid, gid, hash, selected);
  }
  return hash;
}

function rename(uid, gid, wid, newName) {
  // TODO: merge with removeWish
  const users = mongo.db.collection('user');
  users.findOne({ _id: uid }, (err, doc) => {
    const wishGroups = doc.wishGroups;
    for (let i = 0; i < wishGroups.length; i++) {
      const wishGroup = wishGroups[i];
      const wishGroupLength = wishGroup.wishes ? wishGroup.wishes.length : 0;
      for (let j = 0; j < wishGroupLength; j++) {
        const wish = wishGroup.wishes[j];
        if (wishGroup.id === gid && wish.id === wid) {
          wish.name = newName;
        }
      }
    }
    users.updateOne(
      { _id: uid },
      {
        $set: { wishGroups },
      },
    );
  });
}

function move(uid, gid, wid, newIndex) {
  const users = mongo.db.collection('user');
  users.findOne({ _id: uid },
    (err, doc) => {
      const wishGroups = doc.wishGroups;
      for (let i = 0; i < wishGroups.length; i++) {
        const wishGroup = wishGroups[i];
        const wishGroupLength = wishGroup.wishes ? wishGroup.wishes.length : 0;
        for (let j = 0; j < wishGroupLength; j++) {
          const wish = wishGroup.wishes[j];
          if (wishGroup.id === gid && wish.id === wid) {
            wishGroup.wishes.splice(newIndex, 0, wishGroup.wishes.splice(j, 1)[0]);
            break;
          }
        }
      }
      users.updateOne(
        { _id: uid },
        {
          $set: { wishGroups },
        },
      );
    },
  );
}

function remove(uid, gid, wid) {
  // il faut aussi supprimer le wish des selectedWish si il y est.
  const users = mongo.db.collection('user');
  users.findOne({ _id: uid },
    (err, doc) => {
      const wishGroups = doc.wishGroups;
      for (let i = 0; i < wishGroups.length; i++) {
        const wishGroup = wishGroups[i];
        const wishGroupLength = wishGroup.wishes ? wishGroup.wishes.length : 0;
        for (let j = 0; j < wishGroupLength; j++) {
          const wish = wishGroup.wishes[j];
          if (wishGroup.id === gid && wish.id === wid) {
            wishGroup.wishes.splice(j, 1);
            break;
          }
        }
      }
      users.updateOne(
        { _id: uid },
        {
          $set: { wishGroups },
        },
      );
    },
  );
}

module.exports = {
  add,
  select,
  rename,
  move,
  remove,
};
