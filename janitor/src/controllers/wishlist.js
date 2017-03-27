const router = require('express').Router();
const mid = require('../middlewares');

router.get('/',
  mid.isAuthenticated,
  ({ user }, res) => {
    const currentBasket = user.currentBasket ? user.currentBasket.selectedWishes : {};
    const resp = JSON.stringify({
      wishGroups: user.wishGroups,
      currentBasket,
    });
    res.json(resp);
  },
);

router.post('/autoFill',
  mid.isAuthenticated,
  ({ user }, res) => {
    res.send('not implemented yet');
  },
);

router.post('/order',
  mid.isAuthenticated,
  ({ user }, res) => {
    res.send('not implemented yet');
  },
);

module.exports = function init() {
  return router;
};
