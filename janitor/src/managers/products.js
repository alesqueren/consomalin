const mongo = require('../bs/mongo');

function update(uid, gid, wid, pid, quantity) {
  const users = mongo.db.collection('user');
  const path = `currentBasket.selectedWishes.${gid}.${wid}.${pid}`;
  users.updateOne(
    { _id: uid },
    {
      $set: {
        [path]: quantity,
      },
    },
  );
}
function add(uid, gid, wid, pid, quantity) {
  const users = mongo.db.collection('user');
  const path = `currentBasket.selectedWishes.${gid}.${wid}.${pid}`;
  users.updateOne(
    { _id: uid },
    {
      $set: {
        [path]: quantity,
      },
    },
  );
}
function remove(uid, gid, wid, pid) {
  const users = mongo.db.collection('user');
  const path = `currentBasket.selectedWishes.${gid}.${wid}.${pid}`;
  users.updateOne(
    { _id: uid },
    {
      $unset: {
        [path]: '',
      },
    },
  );
}

module.exports = {
  update,
  add,
  remove,
};
