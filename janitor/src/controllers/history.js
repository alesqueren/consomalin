const router = require('express').Router();
const history = require('../bs/history');

router.post('/:history_uri*',
  (req, res) => {
    const sid = req.headers.cookie.substring(14);
    history.send(req.params.history_uri, sid, req.body).then((response) => {
      res.json(response);
    });
  },
);

module.exports = function init() {
  return router;
};