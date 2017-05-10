const express = require('express');
const logger = require('morgan');
const bodyParser = require('body-parser');
const mongo = require('./bs/mongo');
const srv = require('./srv.js');

const app = express();

app.use(logger('tiny'));
app.use(bodyParser.json({ limit: '50mb' }));
app.use(bodyParser.urlencoded({ limit: '50mb' }));

mongo.setConnection.then(
  (newdb) => {
    mongo.db = newdb;
    srv(app);
  },
  (err) => { console.log(err); },
);
