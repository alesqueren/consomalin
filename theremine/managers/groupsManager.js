const db = require('../db');

function addGroup(_idUser, groupName) {
  const users = db.get().collection('users');

  users.updateOne(
    { _id: _idUser },
    {
      $push: {
        wishGroups: { name: groupName },
      },
    },
    // (err) => {
    //   console.log(`err: ${err}`);
    // });
    );
}

module.exports = {
  add: addGroup,
};
