const router = require('express').Router();
const mid = require('../middlewares');
const productManager = require('../managers/products');

router.post('/groups/:gid/wishes/:wid/product',
  mid.isAuthenticated,
  mid.checkWish,
  mid.parseData({
    productId: {
      required: true,
    },
    quantity: {
      required: false,
      type: 'int',
    },
  }),
  ({ params, data, user }, res) => {
    productManager.set(user._id, params.gid, params.wid, data.productId);
    if (data.quantity) {
      productManager.setQuantity(user._id, params.gid, params.wid, data.quantity);
    }
    res.json('OK');
  },
);

router.put('/groups/:gid/wishes/:wid/product',
  mid.isAuthenticated,
  ({ params, data, user }, res) => {
    productManager.setProductQty(user._id, params.gid, params.wid, data.quantity);
    res.json('OK');
  },
);

router.delete('/groups/:gid/wishes/:wid/product',
  mid.isAuthenticated,
  ({ params, data, user }, res) => {
    productManager.remove(user._id, params.gid, params.wid);
    res.json('OK');
  },
);

module.exports = function init() {
  return router;
};
