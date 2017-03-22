const LocalStrategy = require('passport-local').Strategy;
const bCrypt = require('bcrypt-nodejs');
const usersManager = require('../managers/usersManager');

function isValidPassword(user, password) {
  return bCrypt.compareSync(password, user.password);
}

module.exports = function init(passport) {
  passport.use('login',
    new LocalStrategy({
      passReqToCallback: true,
    },

    (req, username, password, callback) => {
      const email = username;

      usersManager.find(email, (user) => {
        if (!user) {
          return callback(null, false);
        }
        if (!isValidPassword(user, password)) {
          return callback(null, false); // redirect back to login page
        }
        return callback(null, user);
      });
    }));
};
