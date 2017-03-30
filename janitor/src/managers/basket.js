const mongo = require('../bs/mongo');

const userCollectionName = 'user';

function setCurrentWish(email, gid, wid) {
  const users = mongo.db.collection(userCollectionName);
  const path = 'currentBasket.currentWish';
  users.updateOne(
    { _id: email },
    {
      $set: {
        [path]: {
          gid,
          wid,
        },
      },
    },
  );
}
function removeCurrentWish(email) {
  const users = mongo.db.collection(userCollectionName);
  const path = 'currentBasket.currentWish';
  users.updateOne(
    { _id: email },
    {
      $set: {
        [path]: {},
      },
    },
  );
}
// function setCurrentSlot(email, slot) {
//   const users = mongo.db.collection(userCollectionName);
//   const path = 'currentBasket.currentSlot';
//   users.updateOne(
//     { _id: email },
//     {
//       $set: {
//         [path]: slot,
//       },
//     },
//   );
// }

module.exports = {
  setCurrentWish,
  removeCurrentWish,
};
