const router = require('express').Router();
const mid = require('../middlewares');
const basketManager = require('../managers/basket');

router.post('/basket/currentWish',
  mid.isAuthenticated,
  mid.parseData({
    groupId: { required: true },
    wishId: { required: true },
  }),
  ({ data, user }, res) => {
    basketManager.setCurrentWish(user._id, data.groupId, data.wishId);
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
