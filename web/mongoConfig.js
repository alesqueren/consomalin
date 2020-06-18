const mongoURL = process.env.MONGOURL || 'mongodb://localhost:27017/drive_web';

module.exports = {
  url: mongoURL,
};
