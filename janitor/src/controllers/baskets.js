const router = require('express').Router();
const mid = require('../middlewares');
const basketsManager = require('../managers/baskets');
const transactionsManager = require('../managers/transactions');
const rabbitMQ = require('../bs/rabbitMQ');
const kiva = require('../bs/kiva');
const wendy = require('../bs/wendy');

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

router.post('/basket/slot',
  mid.isAuthenticated,
  mid.parseData({
    id: { required: true },
    dateTime: { required: true },
  }),
  ({ data, user }, res) => {
    const dateTime = new Date(data.dateTime);
    basketsManager.setCurrentSlot(user._id, { id: data.id, dateTime });
    res.json('OK');
  },
);

router.post('/basket/prepareOrder',
  mid.isAuthenticated,
  mid.parseData({
    basket: { required: true },
    slotId: { required: true },
  }),
  ({ data, user }, res) => {
    const wendyUrl = 'user/' + user._id + '/prepareOrder';
    const wendyData = { basket: data.basket, slotId: data.slotId };
    wendy.send(wendyUrl, wendyData).then((result) => {
      res.json(result);
    });
  },
);

// TODO: refactor in manager
router.post('/basket/order',
  mid.isAuthenticated,
  mid.parseData({
    basket: { required: true },
    slotId: { required: true },
  }),
  ({ data, user }, res) => {
    const idUser = user._id;
    const slotId = user.currentBasket.slot.id;
    const slotDateTime = user.currentBasket.slot.dateTime;

    const wendyUrl = 'user/' + user._id + '/order';
    const wendyData = { basket: data.basket, slotId: data.slotId };
    wendy.send(wendyUrl, wendyData).then((result) => {
      console.log(result);
      const wishGroups = user.wishGroups;
      const pSelectedWishes = user.currentBasket.selectedWishes;

      const productsToDetail = [];
      const wishGroupsToSave = [];
      for (let i = 0; i < wishGroups.length; i++) {
        const wishGroup = wishGroups[i];
        const wishGroupLength = wishGroup.wishes ? wishGroup.wishes.length : 0;
        const wishesToInsert = [];
        for (let j = 0; j < wishGroupLength; j++) {
          const wish = wishGroup.wishes[j];
          if (pSelectedWishes[wishGroup.id] && pSelectedWishes[wishGroup.id][wish.id]) {
            const products = pSelectedWishes[wishGroup.id][wish.id];
            for (let k = 0; k < products.length; k++) {
              const product = products[k];
              const wishToInsert = {
                name: wish.name,
                product: { id: product.pid },
                quantity: parseInt(product.quantity, 10),
              };
              wishesToInsert.push(wishToInsert);
              productsToDetail.push(product.pid);
            }
          }
        }
        if (wishesToInsert.length) {
          wishGroupsToSave.push({ name: wishGroup.name, wishes: wishesToInsert });
        }
      }

      const detailsUrl = 'details?pids=' + JSON.stringify(productsToDetail);
      kiva.send(detailsUrl).then((products) => {
        products = JSON.parse(products);
        for (let i = 0; i < wishGroupsToSave.length; i++) {
          const wishGroup = wishGroupsToSave[i];
          const wishGroupLength = wishGroup.wishes ? wishGroup.wishes.length : 0;
          for (let j = 0; j < wishGroupLength; j++) {
            const wish = wishGroup.wishes[j];
            const product = products[wish.product.id];
            wish.product.image = product.imageUrl;
          }
        }
      });
      const transactionId = transactionsManager.add(idUser, slotId, slotDateTime, result);
      res.json(result);
    });
  },
);

module.exports = function init() {
  return router;
};
