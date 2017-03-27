const login = require('./login');
const register = require('./register');
const usersManager = require('../managers/users');

module.exports = function init(passport) {
  // Passport needs to be able to serialize and deserialize users to
  // support persistent login sessions
  passport.serializeUser((user, callback) => {
    callback(null, user._id);
  });

  passport.deserializeUser((id, callback) => {
    usersManager.find(id, (user) => {
      callback(null, user);
    });
  });

  login(passport);
  register(passport);
};
