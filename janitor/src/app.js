const express = require('express');
const path = require('path');
const logger = require('morgan');
const cookieParser = require('cookie-parser');
const bodyParser = require('body-parser');
const mongo = require('./bs/mongo');
const srv = require('./srv.js');

const app = express();

app.use(logger('tiny'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded());
app.use(cookieParser());
app.use(express.static(path.join(__dirname, 'public')));

// Configuring Passport
const passport = require('passport');
const expressSession = require('express-session');
const MongoStore = require('connect-mongo')(expressSession);

app.use(expressSession({
  secret: 'somethingimportantandsecret',
  cookie: { maxAge: 60000 * 60 * 24 },
  store: new MongoStore({
    url: mongo.url,
  }),
}));
app.use(passport.initialize());
app.use(passport.session());

// Using the flash middleware
const flash = require('connect-flash');

app.use(flash());

// Initialize Passport
const initPassport = require('./passport/init');

initPassport(passport);

mongo.setConnection.then(
  (newdb) => {
    mongo.db = newdb;
    srv(app);
  },
  (err) => { console.log(err); },
);
