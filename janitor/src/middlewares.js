/* eslint eqeqeq: "off"  */

function isAuthenticated(req, res, next) {
  if (req.isAuthenticated()) {
    return next();
  }
  res.send(401, 'Unauthorized\n');
  return null;
}

function checkGroup({ params, user }, res, next) {
  if (user.wishGroups.filter(wg => wg.id === params.gid).length > 0) {
    return next();
  }
  res.send(422);
  return null;
}

function checkWish({ params, user }, res, next) {
  return next();
}

function isInt(value) {
  return !isNaN(value) &&
    parseInt(Number(value), 10) == value &&
    !isNaN(parseInt(value, 10));
}

const parseData = fields => (req, res, next) => {
  req.data = {};
  for (const field in fields) {
    const value = req.body[field];
    if (fields[field].required && !value) {
      return res.send(400);
    }
    if (fields[field].type === 'int' && !isInt(value)) {
      return res.send(400);
    }
    if (value) {
      try {
        req.data[field] = JSON.parse(value);
      } catch (e) {
        return res.send(400);
      }
    }
  }
  return next();
};

// const parseInt = field => ({ data }, res, next) => {
//   if (data[field]) {
//     try {
//       const value = parseInt(data[field], 10);
//       data[field] = value;
//       return next();
//     } catch (e) {
//       return res.send(400);
//     }
//   }
//   return next();
// };

module.exports = {
  isAuthenticated,
  checkGroup,
  checkWish,
  parseData,
  // parseInt,
};
