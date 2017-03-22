const router = require('express').Router();

const isAuthenticated = function (req, res, next) {
  if (req.isAuthenticated()) {
    return next();
  }
  res.send(401, 'Unauthorized');
}

module.exports = function init(passport) {

  // TODO: allow not logged users
  router.get('/user', isAuthenticated, 
      (req, res) => {
        res.send(JSON.stringify(req.user._id));
      }
  );

  router.post('/users/login',
    passport.authenticate('login'),
      (req, res) => {
        res.send("success");
      }
  );

  router.post('/users/register', 
      passport.authenticate('register'),
      (req, res) => {
        res.send("success");
      }
  );

  router.get('/users/signout', (req, res) => {
    req.logout();
    res.send("success");
  });

  return router;
};
