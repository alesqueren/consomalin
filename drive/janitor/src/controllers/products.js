const router = require('express').Router();
const mid = require('../middlewares');
const kiva = require('../bs/kiva');

router.get('/:kiva_uri*',
  mid.isAuthenticated,
  ({ params }, res) => {
    kiva.send(params.kiva_uri).then((response) => {
      res.json(response);
    });
  },
);

module.exports = function init() {
  return router;
};
