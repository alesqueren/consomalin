const mongoDB = 'users';
const mongoURL = process.env.MONGO_URI || 'mongodb://localhost:27017';

module.exports = {
  url: mongoURL + '/' + mongoDB,
};
