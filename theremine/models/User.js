const mongoose = require('mongoose');

const Schema = mongoose.Schema;

const User = new Schema({
  _id: { type: String },
  password: String,
});

User.virtual('email').get(function get() {
  // return this._id;
  return this.id;
});

module.exports = mongoose.model('User', User);
