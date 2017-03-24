const router = require('express').Router();
const mid = require('../middlewares');
const groupsManager = require('../managers/groups');
const wishesManager = require('../managers/wishes');

router.post('/groups',
  mid.isAuthenticated,
  mid.parseData({
    name: { required: true },
  }),
  ({ data, user }, res) => {
    const groupId = groupsManager.add(user._id, data.name);
    res.json(groupId);
  },
);

router.put('/groups/:gid',
  mid.isAuthenticated,
  mid.checkGroup,
  ({ params, data, user }, res) => {
    const gid = params.gid;
    if (data.selected) {
      for (const wid in user.wishGroups[gid]) {
        // TODO: test
        // console.log(user.wishGroups[gid]);
        // console.log(user.wishGroups[gid][wid]);
        // console.log(wid);
        const wishId = user.wishGroups[gid][wid];
        wishesManager.select(user._id, gid, wishId, data.selected);
      }
    }
    if (data.name) {
      groupsManager.rename(user._id, gid, data.name);
    }
    res.json('OK');
  },
);

module.exports = function init() {
  return router;
};
