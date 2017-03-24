const router = require('express').Router();
const isAuthenticated = require('../passport/auth');
const groupsManager = require('../managers/groups');
const wishesManager = require('../managers/wishes');

router.post('/',
  isAuthenticated,
  ({ body, user }, res) => {
    const name = body.name;
    if (!name) {
      res.send(400);
    } else {
      const groupId = groupsManager.add(user._id, name);
      res.json(groupId);
    }
  },
);

router.put('/:gid',
  isAuthenticated,
  ({ params, body, user }, res) => {
  // (req, res) => {
    // const params = req.params;
    // const body = req.body;
    // const user = req.user;
    // const send = res.send;
    // console.log(req);
    // console.log(res);
    // console.log(res.send);
    // console.log(res.send(200));

    const gid = params.gid;

    if (!body.selected && !body.name) {
      res.send(400);
      return;
    }

    if (body.selected) {
      for (const wid in user.wishGroups[gid]) {
        const wishId = user.wishGroups[gid][wid];
        wishesManager.select(user._id, gid, wishId, body.selected);
      }
    }
    if (body.name) {
      groupsManager.rename(user._id, gid, body.name);
    }
    res.json('OK');
  },
);

router.delete('/:gid',
  isAuthenticated,
  (req, res) => {
    groupsManager.remove(req.user._id, req.params.gid);
    res.json('OK');
  },
);

module.exports = function init() {
  return router;
};
