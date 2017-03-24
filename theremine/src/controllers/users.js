const passport = require('passport');
const router = require('express').Router();
const isAuthenticated = require('../passport/auth');

// TODO: allow not logged users
router.get('/me',
  isAuthenticated,
  (req, res) => {
    res.send(req.user._id);
  },
);

router.post('/login',
  passport.authenticate('login'),
  (req, res) => {
    res.json('success');
  },
);

router.post('/register',
  passport.authenticate('register'),
  (req, res) => {
    res.json('success');
  },
);

router.get('/signout',
  (req, res) => {
    req.logout();
    res.json('success');
  },
);

module.exports = function init() {
  return router;
};
