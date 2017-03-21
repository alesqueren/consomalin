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
          // console.log('user.email : ' + user.email);
          if (!user) {
            // console.log('User Not Found with email '+email);
            return callback(null, false);
          }
          // User exists but wrong password, log the error
          if (!isValidPassword(user, password)) {
            // console.log('Invalid Password');
            return callback(null, false); // redirect back to login page
          }
          return callback(null, user);
        });

        // check in mongo if a user with email exists or not
        // User.findOne({ '_id' :  email },
        //     function(err, user) {
        //         // In case of any error, return using the done method
        //         if (err)
        //             return done(err);
        //         // Username does not exist, log the error and redirect back
        //         if (!user){
        //             console.log('User Not Found with email '+email);
        //             return done(null, false, req.flash('message', 'User Not found.'));
        //         }
        //         // User exists but wrong password, log the error
        //         if (!isValidPassword(user, password)){
        //             console.log('Invalid Password');
        //             // redirect back to login page
        //             return done(null, false, req.flash('message', 'Invalid Password'));
        //         }
        //         // User and password both match, return user from done method
        //         // which will be treated like success
        //         return done(null, user);
        //     }
        // );
      }));
};
