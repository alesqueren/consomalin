function isAuthenticated(req, res, next) {
  if (req.isAuthenticated()) {
    return next();
  }
  res.send(401, 'Unauthorized\n');
  return null;
}

module.exports = isAuthenticated;
