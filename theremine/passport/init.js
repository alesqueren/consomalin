const login = require('./login');
const register = require('./register');
const usersManager = require('../managers/usersManager');

module.exports = function init(passport) {
  // Passport needs to be able to serialize and deserialize users to
  // support persistent login sessions
  passport.serializeUser((user, callback) => {
    // console.log('serializing user ' + user._id);
    callback(null, user._id);
  });

  passport.deserializeUser((id, callback) => {
    usersManager.find(id, (user) => {
      // console.log('deserializing user : ',user);
      callback(null, user);
    });
  });

  // Setting up Passport Strategies for Login and SignUp/Registration

  login(passport);
  register(passport);
};
