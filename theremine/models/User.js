const mongoose = require('mongoose');
const Shema = Schema = mongoose.Schema;
const WishGroup = require('./WishGroup');

const User = new Schema({
    _id: {type: String},
    password: String
});

User.virtual('email').get(function() {
    return this._id;
});

module.exports = mongoose.model('User', User);
