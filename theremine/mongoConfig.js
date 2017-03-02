const mongoURL = process.env.MONGO_URI || 'mongodb://localhost:27017/theremine';

module.exports = {
  url: mongoURL,
};
