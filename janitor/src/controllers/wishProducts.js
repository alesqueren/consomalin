const router = require('express').Router();
const mid = require('../middlewares');
const productManager = require('../managers/products');

router.post('/groups/:gid/wishes/:wid/product',
  mid.isAuthenticated,
  mid.checkWish,
  mid.parseData({
    pid: { required: true },
    quantity: { type: 'int' },
  }),
  ({ params, data, user }, res) => {
    const quantity = data.quantity || 1;
    productManager.set(user._id, params.gid, params.wid, data.pid, quantity);
    res.json('OK');
  },
);

router.put('/groups/:gid/wishes/:wid/product',
  mid.isAuthenticated,
  mid.checkWish,
  mid.parseData({
    pid: { required: true },
    quantity: { type: 'int' },
  }),
  ({ params, data, user }, res) => {
    const quantity = data.quantity || 1;
    productManager.set(user._id, params.gid, params.wid, data.pid, quantity);
    res.json('OK');
  },
);

router.delete('/groups/:gid/wishes/:wid/product',
  mid.isAuthenticated,
  mid.checkWish,
  ({ params, data, user }, res) => {
    productManager.remove(user._id, params.gid, params.wid);
    res.json('OK');
  },
);

module.exports = function init() {
  return router;
};