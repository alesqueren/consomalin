const router = require('express').Router();
const mongo = require('../bs/mongo');

const collectionName = 'records';

function add(sid, newStates) {
  const records = mongo.db.collection(collectionName);
  records.updateOne(
    { _id: sid },
    {
      $push: {
        states: newStates,
      },
    },
    { upsert: true },
  );
}

router.post('/sessions/:sid/add',
  ({ params, body }, res) => {
    add(params.sid, body);
    res.json('OK');
  },
);

module.exports = function init() {
  return router;
};
