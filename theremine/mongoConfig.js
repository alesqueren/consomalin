const mongoDB = process.env.MONGO_DB_NAME || 'theremine';
const mongoURL = process.env.MONGO_URI || 'mongodb://localhost:27017';

module.exports = {
  url: mongoURL + '/' + mongoDB,
};
