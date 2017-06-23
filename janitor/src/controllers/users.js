const passport = require('passport');
const router = require('express').Router();
const mid = require('../middlewares');
const rabbitMQ = require('../bs/rabbitMQ');

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
    // const data = {
    //   user: req.body.username,
    // };
    // rabbitMQ.send(JSON.stringify(data), null);
    res.json('OK');
  },
);

router.put('/signout',
  mid.isAuthenticated,
  (req, res) => {
    req.logout();
    res.json('OK');
  },
);

module.exports = function init() {
  return router;
};
