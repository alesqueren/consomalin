const crypto = require('crypto');

function randHash(x, y) {
  const s = x + Date.now().toString();
  return crypto.createHmac('sha256', y).update(s).digest('hex');
}

module.exports = {
  randHash,
};
