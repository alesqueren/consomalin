const router = require('express').Router();
const history = require('../bs/history');

// TODO: * doesn't work for multiple words
router.get('/sessions/:sid',
  (req, res) => {
    history.sendGet(req.params.sid).then((response) => {
      res.json(response);
    });
  },
);

router.post('/:history_uri*',
  (req, res) => {
    const sid = req.headers.cookie.substring(14)
      .split('/')
      .join('_')
      .split('.')
      .join('_');
    history.sendPost(req.params.history_uri, sid, req.body).then((response) => {
      res.json(response);
    });
  },
);

module.exports = function init() {
  return router;
};
