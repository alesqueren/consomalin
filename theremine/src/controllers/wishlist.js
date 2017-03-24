const router = require('express').Router();
const isAuthenticated = require('../passport/auth');

router.get('/',
  isAuthenticated,
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
  isAuthenticated,
  ({ user }, res) => {
    res.send('not implemented yet');
  },
);

router.post('/order',
  isAuthenticated,
  ({ user }, res) => {
    res.send('not implemented yet');
  },
);

module.exports = function init() {
  return router;
};
