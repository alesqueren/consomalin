const LocalStrategy = require('passport-local').Strategy;
const bCrypt = require('bcrypt-nodejs');
// const groupsManager = require('../managers/groupsManager');
const usersManager = require('../managers/usersManager');

// Generates hash using bCrypt
function createHash(password) {
  return bCrypt.hashSync(password, bCrypt.genSaltSync(10), null);
}

module.exports = function init(passport) {
  passport.use('register',
      new LocalStrategy({
        passReqToCallback: true, // allows us to pass back the entire request to the callback
      },
      (req, username, password, callback) => {
        const email = username;
        const hashedPw = createHash(password);

        usersManager.add(email, hashedPw, (newuser) => {
          // console.log('newuser : ' + newuser);
          // console.log('newuser.email : ' + newuser.email);
          callback(null, newuser);
        });
      }));
};
