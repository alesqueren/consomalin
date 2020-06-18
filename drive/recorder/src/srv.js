const router = require('express').Router();
const controller = require('./controllers/history')();

const port = process.env.SERVER_PORT || 3001;

const notFound = router.get('*',
  (req, res) => {
    res.send(404, 'Not found\n');
  });

const server = (app) => {
  app.use('/sessions/', controller);
  app.use('/', notFound);

  app.listen(port, () => {
    console.log(`Listening on port ${port}...`);
  });
};

module.exports = server;
