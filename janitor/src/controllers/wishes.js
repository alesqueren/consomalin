const router = require('express').Router();
const mid = require('../middlewares');
const wishesManager = require('../managers/wishes');
const groupsManager = require('../managers/groups');

router.post('/groups/:gid/wishes/bulk',
  mid.isAuthenticated,
  mid.checkGroup,
  mid.parseData({
    names: { required: true },
  }),
  ({ params, data, user }, res) => {
    const resp = [];
    for (let i = 0; i < data.names.length; i += 1) {
      resp.push(wishesManager.add(user._id, params.gid, data.names[i]));
    }
    res.json(resp);
  },
);

router.post('/groups/:gid/wishes/:wid/move',
  mid.isAuthenticated,
  mid.checkWish,
  mid.parseData({
    index: {
      required: true,
      type: 'int',
    },
  }),
  ({ params, data, user }, res) => {
    wishesManager.move(user._id, params.gid, params.wid, data.index);
    res.json('OK');
  },
);

router.put('/groups/:gid/wishes/:wid',
  mid.isAuthenticated,
  mid.checkWish,
  mid.parseData({
    name: {},
    selected: {},
  }),
  ({ params, data, user }, res) => {
    if (data.name) {
      wishesManager.rename(user._id, params.gid, params.wid, data.name);
    }
    if (data.selected !== undefined) {
      if (!data.selected &&
          Object.keys(user.currentBasket.selectedWishes[params.gid]).length <= 1) {
        groupsManager.remove(user._id, params.gid);
      } else {
        wishesManager.select(user._id, params.gid, params.wid, data.selected);
      }
    }
    res.json('OK');
  },
);

router.delete('/groups/:gid/wishes/:wid',
  mid.isAuthenticated,
  mid.checkWish,
  ({ params, user }, res) => {
    wishesManager.remove(user._id, params.gid, params.wid);
    res.json('OK');
  },
);

module.exports = function init() {
  return router;
};
