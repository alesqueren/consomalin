const LocalStrategy = require('passport-local').Strategy;
const bCrypt = require('bcrypt-nodejs');
const usersManager = require('../managers/users');

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
        if (!user || !isValidPassword(user, password)) {
          return callback(null, false);
        }
        return callback(null, user);
      });
    }));
};
