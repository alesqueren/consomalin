/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId])
/******/ 			return installedModules[moduleId].exports;
/******/
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// identity function for calling harmony imports with the correct context
/******/ 	__webpack_require__.i = function(value) { return value; };
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, {
/******/ 				configurable: false,
/******/ 				enumerable: true,
/******/ 				get: getter
/******/ 			});
/******/ 		}
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = 20);
/******/ })
/************************************************************************/
/******/ ([
/* 0 */
/***/ (function(module, exports) {

module.exports = require("express");

/***/ }),
/* 1 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var _promise = __webpack_require__(31);

var _promise2 = _interopRequireDefault(_promise);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var MongoClient = __webpack_require__(32).MongoClient;

var mongoDB = 'users';
var mongoURL = process.env.MONGO_URI || 'mongodb://localhost:27017/';

function connect(resolve, reject) {
  MongoClient.connect(mongoURL + mongoDB, {}, function (err, db) {
    if (err) {
      reject(err);
    }
    resolve(db);
  });
}

module.exports = {
  setConnection: new _promise2.default(connect),
  url: mongoURL + mongoDB,
  db: null
};

/***/ }),
/* 2 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


function isAuthenticated(req, res, next) {
  if (req.isAuthenticated()) {
    return next();
  }
  res.send(401, 'Unauthorized\n');
  return null;
}

function checkGroup(_ref, res, next) {
  var params = _ref.params,
      user = _ref.user;

  if (user.wishGroups.filter(function (wg) {
    return wg.id === params.gid;
  }).length > 0) {
    return next();
  }
  res.send(422);
  return null;
}

function checkWish(_ref2, res, next) {
  var params = _ref2.params,
      user = _ref2.user;

  return next();
}

function isInt(value) {
  return !isNaN(value) && parseInt(Number(value), 10) == value && !isNaN(parseInt(value, 10));
}

var parseData = function parseData(fields) {
  return function (req, res, next) {
    req.data = {};
    for (var field in fields) {
      var value = req.body[field];
      if (fields[field].required && !value) {
        return res.send(400);
      }
      if (fields[field].type === 'int' && !isInt(value)) {
        return res.send(400);
      }
      if (value) {
        try {
          req.data[field] = JSON.parse(value);
        } catch (e) {
          return res.send(400);
        }
      }
    }
    return next();
  };
};

module.exports = {
  isAuthenticated: isAuthenticated,
  checkGroup: checkGroup,
  checkWish: checkWish,
  parseData: parseData
};

/***/ }),
/* 3 */
/***/ (function(module, exports) {

module.exports = require("babel-runtime/helpers/defineProperty");

/***/ }),
/* 4 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var _defineProperty2 = __webpack_require__(3);

var _defineProperty3 = _interopRequireDefault(_defineProperty2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var mongo = __webpack_require__(1);

var userCollectionName = 'user';

function addUser(email, password, callback) {
  var users = mongo.db.collection(userCollectionName);
  return users.insertOne({
    _id: email,
    password: password
  }, function (err, cursor) {
    if (!err) {
      callback(cursor.ops[0]);
    } else {
      callback(false);
    }
  });
}

function findUser(email, callback) {
  var users = mongo.db.collection(userCollectionName);
  return users.findOne({
    _id: email
  }, function (err, user) {
    callback(user);
  });
}

function setCurrentWish(email, groupId, wishId) {
  var users = mongo.db.collection(userCollectionName);
  var path = 'currentBasket.currentWish';
  users.updateOne({ _id: email }, {
    $set: (0, _defineProperty3.default)({}, path, {
      group: groupId,
      wish: wishId
    })
  });
}

function removeCurrentWish(email) {
  var users = mongo.db.collection(userCollectionName);
  var path = 'currentBasket.currentWish';
  users.updateOne({ _id: email }, {
    $set: (0, _defineProperty3.default)({}, path, {})
  });
}

function setCurrentSlot(email, slot) {
  var users = mongo.db.collection(userCollectionName);
  var path = 'currentBasket.currentSlot';
  users.updateOne({ _id: email }, {
    $set: (0, _defineProperty3.default)({}, path, slot)
  });
}

module.exports = {
  add: addUser,
  find: findUser,
  setCurrentWish: setCurrentWish,
  removeCurrentWish: removeCurrentWish,
  setCurrentSlot: setCurrentSlot
};

/***/ }),
/* 5 */
/***/ (function(module, exports) {

module.exports = require("passport");

/***/ }),
/* 6 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var _defineProperty2 = __webpack_require__(3);

var _defineProperty3 = _interopRequireDefault(_defineProperty2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var mongo = __webpack_require__(1);
var crypto = __webpack_require__(9);

function addGroup(uid, groupName) {
  var users = mongo.db.collection('user');
  var secret = uid;
  var hash = crypto.createHmac('sha256', secret).update(groupName + Date.now().toString()).digest('hex');
  users.updateOne({ _id: uid }, {
    $push: {
      wishGroups: {
        id: hash,
        name: groupName,
        wishes: []
      }
    }
  });
  return hash;
}

function renameGroup(uid, groupId, newName) {
  var users = mongo.db.collection('user');
  var path = 'wishGroups.$.name';
  users.updateOne({ 'wishGroups.id': groupId }, {
    $set: (0, _defineProperty3.default)({}, path, newName)
  });
}

function removeGroup(uid, groupId) {
  var users = mongo.db.collection('user');
  var path = 'wishGroups.$';
  users.updateOne({ 'wishGroups.id': groupId }, {
    $unset: (0, _defineProperty3.default)({}, path, 1)
  });
  users.updateOne({ _id: uid }, {
    $pull: {
      wishGroups: null
    }
  });
}

module.exports = {
  add: addGroup,
  rename: renameGroup,
  remove: removeGroup
};

/***/ }),
/* 7 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var _defineProperty2 = __webpack_require__(3);

var _defineProperty3 = _interopRequireDefault(_defineProperty2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var mongo = __webpack_require__(1);
var utils = __webpack_require__(29);

function select(uid, gid, wid, selected) {
  var users = mongo.db.collection('user');
  var path = 'currentBasket.selectedWishes.' + gid + '.' + wid;
  if (selected) {
    users.updateOne({ _id: uid }, {
      $set: (0, _defineProperty3.default)({}, path, {})
    });
  } else {
    users.updateOne({ _id: uid }, {
      $unset: (0, _defineProperty3.default)({}, path, 1)
    });
    users.updateOne({ _id: uid }, {
      $pull: {
        'currentBasket.selectedWishes': null
      }
    });
  }
}

function add(uid, gid, wishName) {
  var users = mongo.db.collection('user');
  var hash = utils.randHash(uid, wishName);
  var path = 'wishGroups.$.wishes';
  users.updateOne({ 'wishGroups.id': gid }, {
    $push: (0, _defineProperty3.default)({}, path, {
      id: hash,
      name: wishName
    })
  });
  select(uid, gid, hash, true);
  return hash;
}

function rename(uid, gid, wid, newName) {
  var users = mongo.db.collection('user');
  users.findOne({ _id: uid }, function (err, document) {
    var wishGroups = document.wishGroups;
    for (var i = 0; i < wishGroups.length; i++) {
      var wishGroup = wishGroups[i];
      var wishGroupLength = wishGroup.wishes ? wishGroup.wishes.length : 0;
      for (var j = 0; j < wishGroupLength; j++) {
        var wish = wishGroup.wishes[j];
        if (wishGroup.id === gid && wish.id === wid) {
          wish.name = newName;
        }
      }
    }
    users.updateOne({ _id: uid }, {
      $set: { wishGroups: wishGroups }
    });
  });
}

function remove(uid, gid, wid) {
  var users = mongo.db.collection('user');
  users.findOne({ _id: uid }, function (err, document) {
    var wishGroups = document.wishGroups;
    for (var i = 0; i < wishGroups.length; i++) {
      var wishGroup = wishGroups[i];
      var wishGroupLength = wishGroup.wishes ? wishGroup.wishes.length : 0;
      for (var j = 0; j < wishGroupLength; j++) {
        var wish = wishGroup.wishes[j];
        if (wishGroup.id === gid && wish.id === wid) {
          wishGroup.wishes.splice(j, 1);
          break;
        }
      }
    }
    users.updateOne({ _id: uid }, {
      $set: { wishGroups: wishGroups }
    });
  });
}

function move(uid, gid, wid, index) {
  console.log(index);
}

module.exports = {
  add: add,
  select: select,
  rename: rename,
  move: move,
  remove: remove
};

/***/ }),
/* 8 */
/***/ (function(module, exports) {

module.exports = require("bcrypt-nodejs");

/***/ }),
/* 9 */
/***/ (function(module, exports) {

module.exports = require("crypto");

/***/ }),
/* 10 */
/***/ (function(module, exports) {

module.exports = require("passport-local");

/***/ }),
/* 11 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var login = __webpack_require__(27);
var register = __webpack_require__(28);
var usersManager = __webpack_require__(4);

module.exports = function init(passport) {
  passport.serializeUser(function (user, callback) {
    callback(null, user._id);
  });

  passport.deserializeUser(function (id, callback) {
    usersManager.find(id, function (user) {
      callback(null, user);
    });
  });

  login(passport);
  register(passport);
};

/***/ }),
/* 12 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var router = __webpack_require__(0).Router();
var userControllers = __webpack_require__(23)();
var wishListControllers = __webpack_require__(25)();
var groupsControllers = __webpack_require__(21)();
var wishesControllers = __webpack_require__(24)();
var productsControllers = __webpack_require__(22)();

var port = process.env.SERVER_PORT || 3000;

var notFound = router.get('*', function (req, res) {
  res.send('Not found\n');
});

var server = function server(app) {
  app.use('/users/', userControllers);
  app.use('/wishlist/', wishListControllers);
  app.use('/wishlist/', groupsControllers);
  app.use('/wishlist/', wishesControllers);
  app.use('/wishlist/', productsControllers);
  app.use('/', notFound);

  app.listen(port, function () {
    console.log('Listening on port ' + port + '...');
  });
};

module.exports = server;

/***/ }),
/* 13 */
/***/ (function(module, exports) {

module.exports = require("body-parser");

/***/ }),
/* 14 */
/***/ (function(module, exports) {

module.exports = require("connect-flash");

/***/ }),
/* 15 */
/***/ (function(module, exports) {

module.exports = require("connect-mongo");

/***/ }),
/* 16 */
/***/ (function(module, exports) {

module.exports = require("cookie-parser");

/***/ }),
/* 17 */
/***/ (function(module, exports) {

module.exports = require("express-session");

/***/ }),
/* 18 */
/***/ (function(module, exports) {

module.exports = require("morgan");

/***/ }),
/* 19 */
/***/ (function(module, exports) {

module.exports = require("path");

/***/ }),
/* 20 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";
/* WEBPACK VAR INJECTION */(function(__dirname) {

var express = __webpack_require__(0);
var path = __webpack_require__(19);
var logger = __webpack_require__(18);
var cookieParser = __webpack_require__(16);
var bodyParser = __webpack_require__(13);
var mongo = __webpack_require__(1);
var srv = __webpack_require__(12);

var app = express();

app.use(logger('tiny'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded());
app.use(cookieParser());
app.use(express.static(path.join(__dirname, 'public')));

var passport = __webpack_require__(5);
var expressSession = __webpack_require__(17);
var MongoStore = __webpack_require__(15)(expressSession);

app.use(expressSession({
  secret: 'somethingimportantandsecret',
  cookie: { maxAge: 60000 * 60 * 24 },
  store: new MongoStore({
    url: mongo.url
  })
}));
app.use(passport.initialize());
app.use(passport.session());

var flash = __webpack_require__(14);

app.use(flash());

var initPassport = __webpack_require__(11);

initPassport(passport);

mongo.setConnection.then(function (newdb) {
  mongo.db = newdb;
  srv(app);
}, function (err) {
  console.log(err);
});
/* WEBPACK VAR INJECTION */}.call(exports, "/"))

/***/ }),
/* 21 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var router = __webpack_require__(0).Router();
var mid = __webpack_require__(2);
var groupsManager = __webpack_require__(6);
var wishesManager = __webpack_require__(7);

router.post('/groups', mid.isAuthenticated, mid.parseData({
  name: { required: true }
}), function (_ref, res) {
  var data = _ref.data,
      user = _ref.user;

  var groupId = groupsManager.add(user._id, data.name);
  res.json(groupId);
});

router.put('/groups/:gid', mid.isAuthenticated, mid.checkGroup, function (_ref2, res) {
  var params = _ref2.params,
      data = _ref2.data,
      user = _ref2.user;

  var gid = params.gid;
  if (data.selected) {
    for (var wid in user.wishGroups[gid]) {
      var wishId = user.wishGroups[gid][wid];
      wishesManager.select(user._id, gid, wishId, data.selected);
    }
  }
  if (data.name) {
    groupsManager.rename(user._id, gid, data.name);
  }
  res.json('OK');
});

module.exports = function init() {
  return router;
};

/***/ }),
/* 22 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var router = __webpack_require__(0).Router();
var mid = __webpack_require__(2);
var productManager = __webpack_require__(26);

router.post('/groups/:gid/wishes/:wid/product', mid.isAuthenticated, mid.checkWish, mid.parseData({
  productId: {
    required: true
  },
  quantity: {
    required: false,
    type: 'int'
  }
}), function (_ref, res) {
  var params = _ref.params,
      data = _ref.data,
      user = _ref.user;

  productManager.set(user._id, params.gid, params.wid, data.productId);
  if (data.quantity) {
    productManager.setQuantity(user._id, params.gid, params.wid, data.quantity);
  }
  res.json('OK');
});

router.put('/groups/:gid/wishes/:wid/product', mid.isAuthenticated, function (_ref2, res) {
  var params = _ref2.params,
      data = _ref2.data,
      user = _ref2.user;

  productManager.setProductQty(user._id, params.gid, params.wid, data.quantity);
  res.json('OK');
});

router.delete('/groups/:gid/wishes/:wid/product', mid.isAuthenticated, function (_ref3, res) {
  var params = _ref3.params,
      data = _ref3.data,
      user = _ref3.user;

  productManager.remove(user._id, params.gid, params.wid);
  res.json('OK');
});

module.exports = function init() {
  return router;
};

/***/ }),
/* 23 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var passport = __webpack_require__(5);
var router = __webpack_require__(0).Router();
var mid = __webpack_require__(2);

router.get('/me', mid.isAuthenticated, function (req, res) {
  res.json(req.user._id);
});

router.post('/login', passport.authenticate('login'), function (req, res) {
  res.json('OK');
});

router.post('/register', passport.authenticate('register'), function (req, res) {
  res.json('OK');
});

router.get('/signout', function (req, res) {
  req.logout();
  res.json('OK');
});

module.exports = function init() {
  return router;
};

/***/ }),
/* 24 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var router = __webpack_require__(0).Router();
var mid = __webpack_require__(2);
var wishesManager = __webpack_require__(7);

router.post('/groups/:gid/wishes/bulk', mid.isAuthenticated, mid.checkGroup, mid.parseData({
  names: { required: true }
}), function (_ref, res) {
  var params = _ref.params,
      data = _ref.data,
      user = _ref.user;

  var resp = [];
  for (var i = 0; i < data.names.length; i += 1) {
    resp.push(wishesManager.add(user._id, params.gid, data.names[i]));
  }
  res.json(resp);
});

router.post('/groups/:gid/wishes/:wid/move', mid.isAuthenticated, mid.checkWish, mid.parseData({
  index: {
    required: 'mandatory',
    type: 'int'
  }
}), function (_ref2, res) {
  var params = _ref2.params,
      data = _ref2.data,
      user = _ref2.user;

  wishesManager.move(user._id, params.gid, params.wid, data.index);
  res.json('OK');
});

router.delete('/groups/:gid/wishes/:wid', mid.isAuthenticated, mid.checkWish, function (_ref3, res) {
  var params = _ref3.params,
      user = _ref3.user;

  wishesManager.remove(user._id, params.gid, params.wid);
  res.json('OK');
});

module.exports = function init() {
  return router;
};

/***/ }),
/* 25 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var _stringify = __webpack_require__(30);

var _stringify2 = _interopRequireDefault(_stringify);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var router = __webpack_require__(0).Router();
var mid = __webpack_require__(2);

router.get('/', mid.isAuthenticated, function (_ref, res) {
  var user = _ref.user;

  var currentBasket = user.currentBasket ? user.currentBasket.selectedWishes : {};
  var resp = (0, _stringify2.default)({
    wishGroups: user.wishGroups,
    currentBasket: currentBasket
  });
  res.json(resp);
});

router.post('/autoFill', mid.isAuthenticated, function (_ref2, res) {
  var user = _ref2.user;

  res.send('not implemented yet');
});

router.post('/order', mid.isAuthenticated, function (_ref3, res) {
  var user = _ref3.user;

  res.send('not implemented yet');
});

module.exports = function init() {
  return router;
};

/***/ }),
/* 26 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var _defineProperty2 = __webpack_require__(3);

var _defineProperty3 = _interopRequireDefault(_defineProperty2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var mongo = __webpack_require__(1);

function set(uid, gid, wid, pid) {
  var users = mongo.db.collection('user');
  var path = 'currentBasket.selectedWishes.' + gid + '.' + wid;
  users.updateOne({ _id: uid }, {
    $set: (0, _defineProperty3.default)({}, path, {
      pid: pid,
      quantity: 1
    })
  });
}

function setQuantity(uid, gid, wid, qty) {
  var users = mongo.db.collection('user');
  var path = 'currentBasket.selectedWishes.' + gid + '.' + wid + '.product.quantity';
  users.updateOne({ _id: uid }, {
    $set: (0, _defineProperty3.default)({}, path, qty)
  });
}

module.exports = {
  set: set,
  setQuantity: setQuantity
};

/***/ }),
/* 27 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var LocalStrategy = __webpack_require__(10).Strategy;
var bCrypt = __webpack_require__(8);
var usersManager = __webpack_require__(4);

function isValidPassword(user, password) {
  return bCrypt.compareSync(password, user.password);
}

module.exports = function init(passport) {
  passport.use('login', new LocalStrategy({
    passReqToCallback: true
  }, function (req, username, password, callback) {
    var email = username;

    usersManager.find(email, function (user) {
      if (!user || !isValidPassword(user, password)) {
        return callback(null, false);
      }
      return callback(null, user);
    });
  }));
};

/***/ }),
/* 28 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var LocalStrategy = __webpack_require__(10).Strategy;
var bCrypt = __webpack_require__(8);
var usersManager = __webpack_require__(4);
var groupsManager = __webpack_require__(6);

function createHash(password) {
  return bCrypt.hashSync(password, bCrypt.genSaltSync(10), null);
}

module.exports = function init(passport) {
  passport.use('register', new LocalStrategy({
    passReqToCallback: true }, function (req, username, password, callback) {
    var email = username;
    var hashedPw = createHash(password);

    usersManager.add(email, hashedPw, function (newuser) {
      if (newuser) {
        groupsManager.add(email, 'default');
      }
      callback(null, newuser);
    });
  }));
};

/***/ }),
/* 29 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var crypto = __webpack_require__(9);

function randHash(x, y) {
  var s = x + Date.now().toString();
  return crypto.createHmac('sha256', y).update(s).digest('hex');
}

module.exports = {
  randHash: randHash
};

/***/ }),
/* 30 */
/***/ (function(module, exports) {

module.exports = require("babel-runtime/core-js/json/stringify");

/***/ }),
/* 31 */
/***/ (function(module, exports) {

module.exports = require("babel-runtime/core-js/promise");

/***/ }),
/* 32 */
/***/ (function(module, exports) {

module.exports = require("mongodb");

/***/ })
/******/ ]);