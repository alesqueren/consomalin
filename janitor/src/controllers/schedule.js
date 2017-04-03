const router = require('express').Router();
const metronome = require('../bs/metronome');
const mid = require('../middlewares');

// recuperer les slots
router.get('/',
  mid.isAuthenticated,
  (req, res) => {
    metronome.send().then((slots) => {
      res.json(slots);
    });
  },
);

module.exports = function init() {
  return router;
};
