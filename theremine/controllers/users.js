const router = require('express').Router();

/*
const isAuthenticated = function (req, res, next) {
  if (req.isAuthenticated()) {
    return next();
  }
  res.redirect('/');
}
*/

module.exports = function init(passport) {
  // login && registration page
  router.get('/users/loginregister', (req, res) => {
    res.render('users/loginRegister', { message: req.flash('message') });
  });

  // login request
  router.post('/users/login', passport.authenticate('login', {
    successRedirect: '/wishlist',
    failureRedirect: '/users/loginregister',
    failureFlash: true,
  }));

  // registration request
  router.post('/users/register', passport.authenticate('register', {
    successRedirect: '/wishlist',
    failureRedirect: '/users/loginRegister',
    failureFlash: true,
  }));

  router.get('/users/signout', (req, res) => {
    req.logout();
    res.redirect('/users/loginregister');
  });

  return router;
};
