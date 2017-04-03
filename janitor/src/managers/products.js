const mongo = require('../bs/mongo');

function set(uid, gid, wid, pid, quantity) {
  const users = mongo.db.collection('user');
  const path = `currentBasket.selectedWishes.${gid}.${wid}`;
  users.updateOne(
    { _id: uid },
    {
      $set: {
        [path]: {
          pid,
          quantity,
        },
      },
    },
  );
}

module.exports = {
  set,
};
