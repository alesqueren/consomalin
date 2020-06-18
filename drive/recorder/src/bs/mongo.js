const MongoClient = require('mongodb').MongoClient;

const mongoDB = 'users';
const mongoURL = process.env.MONGO_URI || 'mongodb://localhost:27017/';

function connect(resolve, reject) {
  MongoClient.connect(mongoURL + mongoDB, {},
    (err, db) => {
      if (err) {
        reject(err);
      }
      resolve(db);
    });
}

module.exports = {
  setConnection: new Promise(connect),
  url: mongoURL + mongoDB,
  db: null,
};
