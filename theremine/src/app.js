const express = require('express');
const path = require('path');
const favicon = require('static-favicon');
const logger = require('morgan');
const cookieParser = require('cookie-parser');
const bodyParser = require('body-parser');
const mongo = require('./bs/mongo');

const app = express();

// view engine setup
app.set('../views', path.join(__dirname, 'views'));
app.set('view engine', 'pug');

app.use(favicon('public/images/favicon.ico'));
//app.use(logger('dev'));
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
    secret:'somethingimportantandsecret',
    cookie:
      {
        maxAge: 60000 * 60 * 24 // 1 day
      },
    store:
      new MongoStore({
        url: mongo.url
      })
}));
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
const wishListControllers = require('./controllers/wishlist')(passport);
const sectionControllers = require('./controllers/section')(passport);
const productsControllers = require('./controllers/products')(passport);
const groupsControllers = require('./controllers/groups')(passport);
const wishesControllers = require('./controllers/wishes')(passport);
const basketControllers = require('./controllers/basket')(passport);
const withdrawControllers = require('./controllers/withdraw')(passport);
const confirmationControllers = require('./controllers/confirmation')(passport);

app.use(function(req, res, next) {
    res.locals.user = req.user;
    next();
});

app.use('/', controllers);
app.use('/', userControllers);
app.use('/', sectionControllers);
app.use('/', wishListControllers);
app.use('/', productsControllers);
app.use('/', groupsControllers);
app.use('/', wishesControllers);
app.use('/', basketControllers);
app.use('/', withdrawControllers);
app.use('/', confirmationControllers);


// catch 404 and forward to error handler
app.use(function(req, res, next){
  res.status(404);

  // respond with html page
  if (req.accepts('html')) {
    res.render('errors/404', { url: req.url });
    return;
  }

  // respond with json
  if (req.accepts('json')) {
    res.send({ error: 'Not found' });
    return;
  }

  // default to plain-text. send()
  res.type('txt').send('Not found');
});
// app.use((req, res, next) => {
//   const err = new Error('Not Found');
//   err.status = 404;
//   next(err);
// });

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
