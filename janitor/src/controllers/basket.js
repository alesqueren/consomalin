const router = require('express').Router();
const mid = require('../middlewares');
const basketManager = require('../managers/basket');

router.get('/basket/currentWish',
  mid.isAuthenticated,
  ({ data, user }, res) => {
    res.json(user.currentBasket.currentWish);
  },
);

router.post('/basket/currentWish',
  mid.isAuthenticated,
  mid.parseData({
    gid: { required: true },
    wid: { required: true },
  }),
  ({ data, user }, res) => {
    basketManager.setCurrentWish(user._id, data.gid, data.wid);
    res.json('OK');
  },
);

router.delete('/basket/currentWish',
  mid.isAuthenticated,
  ({ data, user }, res) => {
    basketManager.removeCurrentWish(user._id);
    res.json('OK');
  },
);

module.exports = function init() {
  return router;
};
