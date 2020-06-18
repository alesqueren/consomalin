const express = require('express');
const path = require('path');
const favicon = require('static-favicon');
const logger = require('morgan');
const cookieParser = require('cookie-parser');
const bodyParser = require('body-parser');

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

app.use(expressSession({ secret: 'myNotSoSecretKey' }));
app.use(passport.initialize());
app.use(passport.session());

 // Using the flash middleware
const flash = require('connect-flash');

app.use(flash());

// Initialize Passport
const initPassport = require('./passport/init');

initPassport(passport);

const controllers = require('./controllers/index')();
const userControllers = require('./controllers/users')(passport);
const wishListControllers = require('./controllers/wishList')(passport);

app.use('/', controllers);
app.use('/users', userControllers);
app.use('/wishList', wishListControllers);

// catch 404 and forward to error handler
app.use((req, res, next) => {
  const err = new Error('Not Found');
  err.status = 404;
  next(err);
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
