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
  router.get('/loginRegister', (req, res) => {
    res.render('users/loginRegister', { message: req.flash('message') });
  });

  // login request
  router.post('/login', passport.authenticate('login', {
    successRedirect: '/wishList',
    failureRedirect: '/users/loginRegister',
    failureFlash: true,
  }));

  // registration request
  router.post('/register', passport.authenticate('register', {
    successRedirect: '/wishList',
    failureRedirect: '/users/loginRegister',
    failureFlash: true,
  }));

  router.get('/signout', (req, res) => {
    req.logout();
    res.redirect('/users/loginRegister');
  });

  return router;
};
