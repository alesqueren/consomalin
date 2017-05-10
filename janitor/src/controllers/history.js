const router = require('express').Router();
const history = require('../bs/history');

function parseSid(sid) {
  return decodeURI(sid)
    .split('%')
    .join('_')
    .split('/')
    .join('_')
    .split('.')
    .join('_');
}

// TODO: * doesn't work for multiple words
router.get('/sessions/:sid',
  (req, res) => {
    const sid = parseSid(req.params.sid);
    history.sendGet(sid).then((response) => {
      res.json(response);
    });
  },
);

router.post('/:history_uri*',
  (req, res) => {
    const sid = parseSid(req.headers.cookie.substring(14));
    history.sendPost(req.params.history_uri, sid, req.body).then((response) => {
      res.json(response);
    });
  },
);

module.exports = function init() {
  return router;
};
