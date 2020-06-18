const mongo = require('../bs/mongo');
const basketsManager = require('../managers/baskets');
const utils = require('../utils');

function addTransaction(_idUser, slotId, slotDateTime, basket) {
  console.log('4.1');
  const users = mongo.db.collection('user');
  const hash = utils.randHash(_idUser, slotId + Date.now().toString());
  const request = 'transactions';

  const transaction = {
    id: hash,
    status: 'done',
    total: basket.total,
    orderTime: new Date(),
    slot: {
      id: slotId,
      dateTime: slotDateTime,
    },
    products: basket.products,
  };
  console.log('4.2');
  basketsManager.removeCurrentselectedWishes(_idUser);
  basketsManager.removeCurrentWish(_idUser);
  basketsManager.removeCurrentSlot(_idUser);

  users.updateOne(
    { _id: _idUser },
    {
      $push: {
        [request]: transaction,
      },
    },
    (err) => {
      console.log(`err: ${err}`);
    });
  console.log('4.3');
  return hash;
}

module.exports = {
  add: addTransaction,
};
