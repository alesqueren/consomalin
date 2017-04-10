// TODO: rm module.exports = function init() { ...
const router = require('express').Router();
const userControllers = require('./controllers/users')();
const wishListControllers = require('./controllers/wishlist')();
const groupsControllers = require('./controllers/groups')();
const wishesControllers = require('./controllers/wishes')();
const wishProductsControllers = require('./controllers/wishProducts')();
const basketsControllers = require('./controllers/baskets')();
const productsControllers = require('./controllers/products')();
const scheduleControllers = require('./controllers/schedule')();

const port = process.env.SERVER_PORT || 3001;

const notFound = router.get('*',
  (req, res) => {
    res.send(404, 'Not found\n');
  });

const server = (app) => {
  app.use('/users/', userControllers);
  app.use('/wishlist/', wishListControllers);
  app.use('/wishlist/', groupsControllers);
  app.use('/wishlist/', wishesControllers);
  app.use('/wishlist/', wishProductsControllers);
  app.use('/wishlist/', basketsControllers);
  app.use('/products/', productsControllers);
  app.use('/schedule/', scheduleControllers);
  app.use('/', notFound);

  app.listen(port, () => {
    console.log(`Listening on port ${port}...`);
  });
};

module.exports = server;
