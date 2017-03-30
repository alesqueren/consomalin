const mongo = require('../bs/mongo');

function set(uid, gid, wid, pid) {
  const users = mongo.db.collection('user');
  const path = `currentBasket.selectedWishes.${gid}.${wid}`;
  users.updateOne(
    { _id: uid },
    {
      $set: {
        [path]: {
          pid,
          quantity: 1,
        },
      },
    },
  );
}

module.exports = {
  set,
};
