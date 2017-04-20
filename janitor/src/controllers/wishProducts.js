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

router.post('/groups/:gid/wishes/:wid/product/bulk',
  mid.isAuthenticated,
  mid.checkWish,
  mid.parseData({
    products: { required: true },
  }),
  ({ params, data, user }, res) => {
    for (let i = 0; i < data.products.length; i += 1) {
      const product = data.products[i];
      const pid = product.pid;
      const quantity = product.quantity || 1;
      productManager.add(user._id, params.gid, params.wid, pid, quantity);
    }

    const quantity = data.quantity || 1;
    res.json('OK');
  },
);

router.put('/groups/:gid/wishes/:wid/product',
  mid.isAuthenticated,
  mid.checkWish,
  mid.parseData({
    pid: { required: true },
    quantity: { required: true },
  }),
  ({ params, data, user }, res) => {
    productManager.update(user._id, params.gid, params.wid, data.pid, data.quantity);

    const quantity = data.quantity || 1;
    res.json('OK');
  },
);

router.delete('/groups/:gid/wishes/:wid/product',
  mid.isAuthenticated,
  mid.checkWish,
  mid.parseData({
    pid: { required: true },
  }),
  ({ params, data, user }, res) => {
    productManager.remove(user._id, params.gid, params.wid, data.pid);
    res.json('OK');
  },
);

module.exports = function init() {
  return router;
};
