const mongo = require('../bs/mongo');

function add(uid, gid, wid, pid, quantity) {
  const users = mongo.db.collection('user');
  const path = `currentBasket.selectedWishes.${gid}.${wid}`;
  users.updateOne(
    { _id: uid },
    {
      $push: {
        [path]: { pid, quantity },
      },
    },
  );
}
function update(uid, gid, wid, pid, quantity) {
  const users = mongo.db.collection('user');
  const pathId = `currentBasket.selectedWishes.${gid}.${wid}.pid`;
  const pathQuantity = `currentBasket.selectedWishes.${gid}.${wid}.$.quantity`;
  users.updateOne(
    { [pathId]: pid },
    {
      $set: {
        [pathQuantity]: quantity,
      },
    },
  );
}
function remove(uid, gid, wid, pid) {
  const users = mongo.db.collection('user');
  const path = `currentBasket.selectedWishes.${gid}.${wid}`;
  users.updateOne(
    { _id: uid },
    {
      $pull: {
        [path]: { pid },
      },
    },
  );
}

module.exports = {
  add,
  update,
  remove,
};
