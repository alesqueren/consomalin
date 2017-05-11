const router = require('express').Router();
const mongo = require('../bs/mongo');

const collectionName = 'records';

function get(sid, resolve) {
  const records = mongo.db.collection(collectionName);
  records.findOne({ _id: sid }, resolve);
}

function add(sid, newStates) {
  const records = mongo.db.collection(collectionName);
  records.updateOne(
    { _id: sid },
    {
      $push: {
        states: {
          $each: newStates,
        },
      },
    },
    { upsert: true },
  );
}

router.get('/:sid',
  ({ params }, res) => {
    get(params.sid, (err, doc) => {
      res.json(doc);
    });
  },
);

router.post('/:sid/add',
  ({ body, params }, res) => {
    add(params.sid, body);
    res.json('OK');
  },
);

module.exports = function init() {
  return router;
};
