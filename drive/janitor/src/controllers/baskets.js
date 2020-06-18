const router = require('express').Router();
const mid = require('../middlewares');
const basketsManager = require('../managers/baskets');
const transactionsManager = require('../managers/transactions');
const kiva = require('../bs/kiva');
const wendy = require('../bs/wendy');

// parcourir les selectedWihes pour creer un nouveau groupe avec les produits en double
function getDuplicatedProducts(user, productsOrdered) {
  const selectedWishes = user.currentBasket.selectedWishes;

  const seenProducts = {};
  const duplicatedProducts = {};
  Object.keys(selectedWishes).map((selectedGroupId) => {
    const selectedGroup = selectedWishes[selectedGroupId];
    Object.keys(selectedGroup).map((selectedWishWid) => {
      const selectedWish = selectedGroup[selectedWishWid];
      for (let i = 0; i < selectedWish.length; i++) {
        const wish = selectedWish[i];
        const pid = wish.pid;
        if (productsOrdered[pid]) {
          if (seenProducts[pid]) {
            duplicatedProducts[pid] = true;
          } else {
            seenProducts[pid] = true;
          }
        }
      }
      return true;
    });
    return true;
  });
  console.log('duplicatedProducts');
  console.log(duplicatedProducts);
  return duplicatedProducts;
}
// parcourir les wishgroups et les wish
// connecter les produits si ils existent au wish courant
function getWishesOrdered(user, duplicatedProducts, productsOrdered) {
  const wishGroups = user.wishGroups;
  const selectedWishes = user.currentBasket.selectedWishes;

  const wishGroupsToSave = [];
  for (let i = 0; i < wishGroups.length; i++) {
    const group = wishGroups[i];
    const gid = group.id;
    const groupLength = group.wishes ? group.wishes.length : 0;
    const wishesToInsert = [];
    for (let j = 0; j < groupLength; j++) {
      const wid = group.wishes[j].id;
      const productsToInsert = {};
      if (selectedWishes[gid] && selectedWishes[gid][wid]) {
        const products = selectedWishes[gid][wid];
        for (let k = 0; k < products.length; k++) {
          const pid = products[k].pid;
          if (productsOrdered[pid] && !duplicatedProducts[pid]) {
            productsToInsert[pid] = {};
          }
        }
        if (Object.keys(productsToInsert).length) {
          wishesToInsert.push({
            name: group.wishes[j].name,
            products: productsToInsert,
          });
        }
      }
    }
    if (wishesToInsert.length) {
      wishGroupsToSave.push({ name: group.name, wishes: wishesToInsert });
    }
  }
  if (Object.keys(duplicatedProducts).length) {
    const wishes = [];
    Object.keys(duplicatedProducts).map((pid) => {
      wishes.push({
        name: 'duplicatedProduct',
        products: { [pid]: {} },
      });
      return true;
    });
    wishGroupsToSave.push({ name: 'duplicatedProducts', wishes });
  }
  return wishGroupsToSave;
}

// merge wishesOrdered with the productsOrdered
function improveWishesOrdered(user, wishesOrdered, productsOrdered) {
  // console.log('productsOrdered');
  // console.log(productsOrdered);
  for (let i = 0; i < wishesOrdered.length; i++) {
    const wishGroup = wishesOrdered[i];
    const wishGroupLength = wishGroup.wishes ? wishGroup.wishes.length : 0;
    for (let j = 0; j < wishGroupLength; j++) {
      const wish = wishGroup.wishes[j];
      Object.keys(wish.products).map((pid) => {
        // console.log('improveWishesOrdered : ' + pid);
        if (productsOrdered[pid]) {
          const po = productsOrdered[pid];
          // console.log('improve');
          wishesOrdered[i].wishes[j].products[pid] = {
            productNb: po.productNb,
            priceByProduct: po.priceByProduct,
            price: po.price,
          };
        }
        return true;
      });
    }
  }
  return wishesOrdered;
}

// get more details from kiva to add img etc ..
function detailWishesOrdered(wishesOrdered, productsOrdered) {
  return new Promise((resolve) => {
    const productsToDetails = [];
    Object.keys(productsOrdered).map((pid) => {
      productsToDetails.push('' + pid);
      return true;
    });
    kiva.send('details?pids=' + JSON.stringify(productsToDetails)).then((productsDetailed) => {
      productsDetailed = JSON.parse(productsDetailed);
      for (let i = 0; i < wishesOrdered.length; i++) {
        const group = wishesOrdered[i];
        const wishGroupLength = group.wishes ? group.wishes.length : 0;
        for (let j = 0; j < wishGroupLength; j++) {
          const wish = group.wishes[j];
          Object.keys(wish.products).map((pid) => {
            if (productsDetailed[pid]) {
              // console.log('detail');
              const product = productsDetailed[pid];
              wishesOrdered[i].wishes[j].products[pid].image = product.imageUrl;
              wishesOrdered[i].wishes[j].products[pid].quantityUnit = product.quantityUnit;
              wishesOrdered[i].wishes[j].products[pid].name = product.name;
            }
            return true;
          });
        }
      }
      resolve(wishesOrdered);
    });
  });
}

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
    wendy.send(wendyUrl, wendyData).then(({ statusCode, result }) => {
      if (statusCode === 200) {
        res.json('OK');
      } else {
        res.json(result);
      }
    });
  },
);

// TODO: refactor in manager
router.post('/basket/order',
  mid.isAuthenticated,
  mid.parseData({
    basket: { required: true },
    slotId: { required: true },
    isDemo: { required: true },
  }),
  ({ data, user }, res) => {
    const idUser = user._id;
    const slotId = user.currentBasket.slot.id;
    const slotDateTime = user.currentBasket.slot.dateTime;
    const isDemo = data.isDemo;

    const wendyUrl = 'user/' + user._id + '/order';
    const wendyData = { basket: data.basket, slotId: data.slotId };
    if (isDemo === false) {
      wendy.send(wendyUrl, wendyData).then(({ statusCode, result }) => {
        if (statusCode !== 200) {
          res.json(result);
        }
        const basketOrdered = data.basket;
        const productsOrdered = basketOrdered.products;
        const total = basketOrdered.totalPrice;
        const transactionId = result.transactionId;
        const duplicatedProducts = getDuplicatedProducts(user, productsOrdered);
        let wishesOrdered = getWishesOrdered(user, duplicatedProducts, productsOrdered);
        wishesOrdered = improveWishesOrdered(user, wishesOrdered, productsOrdered);
        detailWishesOrdered(wishesOrdered, productsOrdered).then((wishGroups) => {
          const basket = {
            transactionId,
            products: wishGroups,
            total,
          };
          transactionsManager.add(idUser, slotId, slotDateTime, basket);
          res.json(transactionId);
        });
      });
    } else {
      const basketOrdered = data.basket;
      const productsOrdered = basketOrdered.products;
      const total = basketOrdered.totalPrice;
      const transactionId = 'commanddetest';
      const duplicatedProducts = getDuplicatedProducts(user, productsOrdered);
      let wishesOrdered = getWishesOrdered(user, duplicatedProducts, productsOrdered);
      wishesOrdered = improveWishesOrdered(user, wishesOrdered, productsOrdered);
      detailWishesOrdered(wishesOrdered, productsOrdered).then((wishGroups) => {
        const basket = {
          transactionId,
          products: wishGroups,
          total,
        };
        transactionsManager.add(idUser, slotId, slotDateTime, basket);
        res.json(transactionId);
      });
    }
  },
);

module.exports = function init() {
  return router;
};
