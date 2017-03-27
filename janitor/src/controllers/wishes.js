const router = require('express').Router();
const mid = require('../middlewares');
const wishesManager = require('../managers/wishes');

router.post('/groups/:gid/wishes/bulk',
  mid.isAuthenticated,
  mid.checkGroup,
  mid.parseData({
    names: {
      required: true,
      json: true,
    },
  }),
  ({ params, data, user }, res) => {
    console.log('coucou');
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
    if (data.selected) {
      wishesManager.select(user._id, params.gid, params.wid, data.selected);
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


// //set current wish
// router.put('/wishlist/groups/:gid/wishes/:wid/current', isAuthenticated, (req, res) => {
//   usersManager.setCurrentWish(req.user._id, req.params.gid, req.params.wid);
//   res.setHeader('Content-Type', 'application/json');
//   res.send(JSON.stringify('OK'));
// });
// //remove current wish
// router.put('/wishlist/removeCurrent', isAuthenticated, (req, res) => {
//   usersManager.removeCurrentWish(req.user._id);
//   res.setHeader('Content-Type', 'application/json');
//   res.send(JSON.stringify('OK'));
// });

module.exports = function init() {
  return router;
};
