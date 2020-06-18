/* eslint eqeqeq: "off"  */

function checkHistory({ params, user }, res, next) {
  if (user.wishGroups.filter(wg => wg.id === params.gid).length > 0) {
    return next();
  }
  res.send(422);
  return null;
}

module.exports = {
  checkHistory,
};
