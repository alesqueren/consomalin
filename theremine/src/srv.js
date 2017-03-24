// const passport = require('passport');
const router = require('express').Router();
const userControllers = require('./controllers/users')();
const wishListControllers = require('./controllers/wishlist')();
const groupsControllers = require('./controllers/groups')();
// const wishListControllers = require('./controllers/wishlist')(passport);
// const sectionControllers = require('./controllers/section')(passport);
// const productsControllers = require('./controllers/products')(passport);
// const wishesControllers = require('./controllers/wishes')(passport);
// const basketControllers = require('./controllers/basket')(passport);
// const withdrawControllers = require('./controllers/withdraw')(passport);
// const confirmationControllers = require('./controllers/confirmation')(passport);

const port = process.env.SERVER_PORT || 3000;

const notFound = router.get('*',
  (req, res) => {
    res.send('Not found\n');
  });

const server = (app) => {
  app.use('/users/', userControllers);
  app.use('/wishlist/', wishListControllers);
  app.use('/groups/', groupsControllers);
  app.use('/', notFound);

  app.listen(port, () => {
    console.log(`Listening on port ${port}...`);
  });
};

module.exports = server;
