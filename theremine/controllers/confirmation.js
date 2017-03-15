const router = require('express').Router();

function isAuthenticated(req, res, next) {
  if (req.isAuthenticated()) {
    next();
    return;
  }
  res.redirect('/');
}

module.exports = function init() {
  router.get('/confirmation', isAuthenticated, (req, res) => {
    const transactionDateTime = new Date(req.user.transactions[req.user.transactions.length -1].slot.dateTime);
    console.log(transactionDateTime);
    res.render('confirmation/confirmation', {transactionDateTime : transactionDateTime});
  });

  return router;
};
