const passport = require('passport');
const router = require('express').Router();
const mid = require('../middlewares');

// TODO: allow not logged users
router.get('/me',
  mid.isAuthenticated,
  (req, res) => {
    res.json({ id: req.user._id });
  },
);

router.post('/login',
  passport.authenticate('login'),
  (req, res) => {
    res.json('OK');
  },
);

router.post('/register',
  passport.authenticate('register'),
  (req, res) => {
    res.json('OK');
  },
);

router.get('/signout',
  mid.isAuthenticated,
  (req, res) => {
    req.logout();
    res.json('OK');
  },
);

module.exports = function init() {
  return router;
};
