const mongoURL = process.env.MONGOURL || 'mongodb://localhost:27017/theremine';

module.exports = {
  url: mongoURL,
};
