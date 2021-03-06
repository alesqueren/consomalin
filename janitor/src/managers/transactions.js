const mongo = require('../bs/mongo');
const basketsManager = require('../managers/baskets');
const utils = require('../utils');

function addTransaction(_idUser, slotId, slotDateTime, basket) {
  const users = mongo.db.collection('user');
  const hash = utils.randHash(_idUser, slotId + Date.now().toString());
  const request = 'transactions';

  const transaction = {
    id: hash,
    status: 'done',
    total: basket.totalPrice,
    orderTime: new Date(),
    slot: {
      id: slotId,
      dateTime: slotDateTime,
    },
    products: basket.products,
  };
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
  return hash;
}

module.exports = {
  add: addTransaction,
};
