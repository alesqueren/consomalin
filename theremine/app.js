const express = require('express');
const path = require('path');
const favicon = require('static-favicon');
const logger = require('morgan');
const cookieParser = require('cookie-parser');
const bodyParser = require('body-parser');
const mongoConfig = require('./mongoConfig');

const app = express();

// view engine setup
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'pug');

app.use(favicon('public/images/favicon.ico'));
app.use(logger('dev'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded());
app.use(cookieParser());
app.use(express.static(path.join(__dirname, 'public')));

// Configuring Passport
const passport = require('passport');
const expressSession = require('express-session');
const MongoStore = require('connect-mongo')(expressSession);

app.use(expressSession({ secret: 'myNotSoSecretKey' }));
app.use(passport.initialize());
console.log('mongoConfig.url : ' + mongoConfig.url);
app.use(passport.session({
    secret:'somethingimportantandsecret',
    cookie:
      {
        maxAge: 60000 * 60 * 24 // 1 day
      },
    store:
      new MongoStore({
        url: mongoConfig.url
      })
}));

 // Using the flash middleware
const flash = require('connect-flash');

app.use(flash());

// Initialize Passport
const initPassport = require('./passport/init');

initPassport(passport);

const controllers = require('./controllers/index')();
const userControllers = require('./controllers/users')(passport);
const wishListControllers = require('./controllers/wishlist')(passport);
const sectionControllers = require('./controllers/section')(passport);
const productsControllers = require('./controllers/products')(passport);
const wishesControllers = require('./controllers/wish')(passport);

app.use('/', controllers);
app.use('/users', userControllers);
app.use('/wishlist', wishListControllers);
app.use('/section', sectionControllers);
app.use('/products', productsControllers);
app.use('/wish', wishesControllers);

// catch 404 and forward to error handler
app.use((req, res, next) => {
  const err = new Error('Not Found');
  err.status = 404;
  // next(err);
});

// development error handler
// will print stacktrace
if (app.get('env') === 'development') {
  app.use((err, req, res) => {
    console.trace();
    res.status(err.status || 500);
    res.render('error', {
      message: err.message,
      error: err,
    });
  });
}

module.exports = app;
