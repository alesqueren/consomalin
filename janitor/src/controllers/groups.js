const router = require('express').Router();
const mid = require('../middlewares');
const groupsManager = require('../managers/groups');

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
  mid.parseData({
    name: {},
    selected: {},
  }),
  ({ params, data, user }, res) => {
    const gid = params.gid;

    if (data.name) {
      groupsManager.rename(user._id, gid, data.name);
    }
    if (data.selected !== undefined) {
      if (data.selected) {
        groupsManager.select(user._id, user.wishGroups, gid);
      } else {
        groupsManager.unselect(user._id, gid);
      }
    }
    res.json('OK');
  },
);

router.delete('/groups/:gid',
  mid.isAuthenticated,
  mid.checkGroup,
  ({ params, user }, res) => {
    groupsManager.remove(user._id, params.gid);
    res.json('OK');
  },
);

module.exports = function init() {
  return router;
};
