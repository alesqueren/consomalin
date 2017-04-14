const express = require('express');
const logger = require('morgan');
const bodyParser = require('body-parser');
const mongo = require('./bs/mongo');
const srv = require('./srv.js');

const app = express();

app.use(logger('tiny'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded());

mongo.setConnection.then(
  (newdb) => {
    mongo.db = newdb;
    srv(app);
  },
  (err) => { console.log(err); },
);
