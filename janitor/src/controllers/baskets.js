const router = require('express').Router();
const mid = require('../middlewares');
const basketsManager = require('../managers/baskets');
const usersManager = require('../managers/users');
const transactionsManager = require('../managers/transactions');
const rabbitMQ = require('../bs/rabbitMQ');
const kiva = require('../bs/kiva');

router.get('/basket/currentWish',
  mid.isAuthenticated,
  ({ data, user }, res) => {
    res.json(user.currentBasket.currentWish);
  },
);

router.post('/basket/currentWish',
  mid.isAuthenticated,
  mid.parseData({
    wid: { required: true },
  }),
  ({ data, user }, res) => {
    basketsManager.setCurrentWish(user._id, data.wid);
    res.json('OK');
  },
);

router.delete('/basket/currentWish',
  mid.isAuthenticated,
  ({ data, user }, res) => {
    basketsManager.removeCurrentWish(user._id);
    res.json('OK');
  },
);

// save the slot
router.put('/basket/slot',
  mid.isAuthenticated,
  mid.parseData({
    id: { required: true },
    dateTime: { required: true },
  }),
  ({ data, user }, res) => {
    usersManager.setCurrentSlot(user._id, { slot_id: data.id, slot_dateTime: data.dateTime });
    res.json('OK');
  },
);

// Passer la commande
router.post('/order',
  mid.isAuthenticated,
  (req, res) => {
    const slotId = req.user.currentBasket.currentSlot.slot_id;
    const slotDateTime = req.user.currentBasket.currentSlot.slot_dateTime;
    const wishGroups = req.user.wishGroups;
    const pSelectedWishes = req.user.currentBasket.selectedWishes;

    const productsToDetail = [];
    const wishGroupsToSave = [];
    for (let i = 0; i < wishGroups.length; i++) {
      const wishGroup = wishGroups[i];
      const wishGroupLength = wishGroup.wishes ? wishGroup.wishes.length : 0;
      const wishesToInsert = [];
      for (let j = 0; j < wishGroupLength; j++) {
        const wish = wishGroup.wishes[j];
        if (pSelectedWishes[wishGroup.id] && pSelectedWishes[wishGroup.id][wish.id]) {
          const product = pSelectedWishes[wishGroup.id][wish.id];
          // console.log('product' )
          // console.log(product )
          const wishToInsert = {
            name: wish.name,
            product: { id: product.id },
            quantity: parseInt(product.quantity, 10),
          };
          wishesToInsert.push(wishToInsert);
          productsToDetail.push(product.id);
        }
      }
      if (wishesToInsert.length) {
        wishGroupsToSave.push({ name: wishGroup.name, wishes: wishesToInsert });
      }
    }

    const detailsUrl = '/details?pids=' + JSON.stringify(productsToDetail);
    kiva.send(detailsUrl).then((products) => {
      products = JSON.parse(products);
      // console.log('body:', products);
      for (let i = 0; i < wishGroupsToSave.length; i++) {
        const wishGroup = wishGroupsToSave[i];
        const wishGroupLength = wishGroup.wishes ? wishGroup.wishes.length : 0;
        for (let j = 0; j < wishGroupLength; j++) {
          const wish = wishGroup.wishes[j];
          const product = products[wish.product.id];
          wish.product.image = product.imageUrl;
        }
      }
      const idUser = req.user._id;
      const transactionId = transactionsManager.add(idUser, slotId, slotDateTime, wishGroupsToSave);
      const data = {
        user: req.user._id,
        transaction: transactionId,
      };
      // comment when dev
      rabbitMQ.send(JSON.stringify(data), null);
      res.json('OK');
    });
  },
);

module.exports = function init() {
  return router;
};
