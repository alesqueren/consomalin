const MongoClient = require('mongodb').MongoClient;

const state = {
  db: null,
};

function connect(url, callback) {
  if (state.db) {
    callback();
    return;
  }

  MongoClient.connect(url, {db : 'drive_web'}, (err, db) => {
    if (err) {
      callback(err);
      return;
    }
    state.db = db
    callback()
  });
}

function get() {
  return state.db;
}


function close(callback) {
  if (state.db) {
    state.db.close((err) => {
      state.db = null;
      state.mode = null;
      callback(err);
    });
  }
}

module.exports = {
  connect,
  get,
  close,
};
