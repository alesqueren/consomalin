const mongoURL = process.env.MONGO_URI || 'mongodb://localhost:27017/drive_web';

module.exports = {
  url: mongoURL,
};
