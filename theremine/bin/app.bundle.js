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


var _promise = __webpack_require__(27);

var _promise2 = _interopRequireDefault(_promise);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var MongoClient = __webpack_require__(28).MongoClient;

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


var _defineProperty2 = __webpack_require__(4);

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
  var request = 'currentBasket.currentWish';
  users.updateOne({ _id: email }, {
    $set: (0, _defineProperty3.default)({}, request, {
      group: groupId,
      wish: wishId
    })
  });
}

function removeCurrentWish(email) {
  var users = mongo.db.collection(userCollectionName);
  var request = 'currentBasket.currentWish';
  users.updateOne({ _id: email }, {
    $set: (0, _defineProperty3.default)({}, request, {})
  });
}

function setCurrentSlot(email, slot) {
  var users = mongo.db.collection(userCollectionName);
  var request = 'currentBasket.currentSlot';
  users.updateOne({ _id: email }, {
    $set: (0, _defineProperty3.default)({}, request, slot)
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
/* 3 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


function isAuthenticated(req, res, next) {
  if (req.isAuthenticated()) {
    return next();
  }
  res.send(401, 'Unauthorized\n');
  return null;
}

module.exports = isAuthenticated;

/***/ }),
/* 4 */
/***/ (function(module, exports) {

module.exports = require("babel-runtime/helpers/defineProperty");

/***/ }),
/* 5 */
/***/ (function(module, exports) {

module.exports = require("passport");

/***/ }),
/* 6 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var _defineProperty2 = __webpack_require__(4);

var _defineProperty3 = _interopRequireDefault(_defineProperty2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var mongo = __webpack_require__(1);
var crypto = __webpack_require__(9);

function addGroup(idUser, groupName) {
  var users = mongo.db.collection('user');
  var secret = idUser;
  var hash = crypto.createHmac('sha256', secret).update(groupName + Date.now().toString()).digest('hex');
  users.updateOne({ _id: idUser }, {
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

function renameGroup(idUser, groupId, newName) {
  var users = mongo.db.collection('user');
  var request = 'wishGroups.$.name';
  users.updateOne({ 'wishGroups.id': groupId }, {
    $set: (0, _defineProperty3.default)({}, request, newName)
  });
}

function removeGroup(idUser, groupId) {
  var users = mongo.db.collection('user');
  var request = 'wishGroups.$';
  users.updateOne({ 'wishGroups.id': groupId }, {
    $unset: (0, _defineProperty3.default)({}, request, 1)
  });
  users.updateOne({ _id: idUser }, {
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
/***/ (function(module, exports) {

module.exports = require("babel-runtime/core-js/json/stringify");

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


var login = __webpack_require__(25);
var register = __webpack_require__(26);
var usersManager = __webpack_require__(2);

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
var userControllers = __webpack_require__(22)();
var wishListControllers = __webpack_require__(23)();
var groupsControllers = __webpack_require__(21)();


var port = process.env.SERVER_PORT || 3000;

var notFound = router.get('*', function (req, res) {
  res.send('Not found\n');
});

var server = function server(app) {
  app.use('/users/', userControllers);
  app.use('/wishlist/', wishListControllers);
  app.use('/groups/', groupsControllers);
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
var isAuthenticated = __webpack_require__(3);
var groupsManager = __webpack_require__(6);
var wishesManager = __webpack_require__(24);

router.post('/', isAuthenticated, function (_ref, res) {
  var body = _ref.body,
      user = _ref.user;

  var name = body.name;
  if (!name) {
    res.send(400);
  } else {
    var groupId = groupsManager.add(user._id, name);
    res.json(groupId);
  }
});

router.put('/:gid', isAuthenticated, function (_ref2, res) {
  var params = _ref2.params,
      body = _ref2.body,
      user = _ref2.user;


  var gid = params.gid;

  if (!body.selected && !body.name) {
    res.send(400);
    return;
  }

  if (body.selected) {
    for (var wid in user.wishGroups[gid]) {
      var wishId = user.wishGroups[gid][wid];
      wishesManager.select(user._id, gid, wishId, body.selected);
    }
  }
  if (body.name) {
    groupsManager.rename(user._id, gid, body.name);
  }
  res.send('OK');
});

router.delete('/:gid', isAuthenticated, function (req, res) {
  groupsManager.remove(req.user._id, req.params.gid);
  res.send('OK');
});

module.exports = function init() {
  return router;
};

/***/ }),
/* 22 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var _stringify = __webpack_require__(7);

var _stringify2 = _interopRequireDefault(_stringify);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var passport = __webpack_require__(5);
var router = __webpack_require__(0).Router();
var isAuthenticated = __webpack_require__(3);

router.get('/me', isAuthenticated, function (req, res) {
  res.send((0, _stringify2.default)(req.user._id));
});

router.post('/login', passport.authenticate('login'), function (req, res) {
  res.send('success');
});

router.post('/register', passport.authenticate('register'), function (req, res) {
  res.send('success');
});

router.get('/signout', function (req, res) {
  req.logout();
  res.send('success');
});

module.exports = function init() {
  return router;
};

/***/ }),
/* 23 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var _stringify = __webpack_require__(7);

var _stringify2 = _interopRequireDefault(_stringify);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var router = __webpack_require__(0).Router();
var isAuthenticated = __webpack_require__(3);

router.get('/', isAuthenticated, function (_ref, res) {
  var user = _ref.user;

  var currentBasket = user.currentBasket ? user.currentBasket.selectedWishes : {};
  var resp = (0, _stringify2.default)({
    wishGroups: user.wishGroups,
    currentBasket: currentBasket
  });
  res.json(resp);
});

router.post('/autoFill', isAuthenticated, function (_ref2, res) {
  var user = _ref2.user;

  res.send('not implemented yet');
});

router.post('/order', isAuthenticated, function (_ref3, res) {
  var user = _ref3.user;

  res.send('not implemented yet');
});

module.exports = function init() {
  return router;
};

/***/ }),
/* 24 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var _defineProperty2 = __webpack_require__(4);

var _defineProperty3 = _interopRequireDefault(_defineProperty2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var mongo = __webpack_require__(1);
var crypto = __webpack_require__(9);

function selectWish(idUser, groupId, wishId, selected) {
  var select = selected;
  var users = mongo.db.collection('user');
  var request = 'currentBasket.selectedWishes.' + groupId + '.' + wishId;
  if (select) {
    users.updateOne({ _id: idUser }, {
      $set: (0, _defineProperty3.default)({}, request, {})
    });
  } else {
    users.updateOne({ _id: idUser }, {
      $unset: (0, _defineProperty3.default)({}, request, 1)
    });
    users.updateOne({ _id: idUser }, {
      $pull: {
        'currentBasket.selectedWishes': null
      }
    });
  }
}

function addWish(idUser, groupId, wishName) {
  var users = mongo.db.collection('user');
  var secret = idUser;
  var hash = crypto.createHmac('sha256', secret).update(wishName + Date.now().toString()).digest('hex');
  var request = 'wishGroups.$.wishes';
  users.updateOne({ 'wishGroups.id': groupId }, {
    $push: (0, _defineProperty3.default)({}, request, {
      id: hash,
      name: wishName
    })
  }, function (err) {
    console.log('err: ' + err);
  });
  selectWish(idUser, groupId, hash, true);
  return hash;
}

function renameWish(idUser, groupId, wishId, newName) {
  var users = mongo.db.collection('user');
  users.findOne({ _id: idUser }, function (err, document) {
    var wishGroups = document.wishGroups;
    for (var i = 0; i < wishGroups.length; i++) {
      var wishGroup = wishGroups[i];
      var wishGroupLength = wishGroup.wishes ? wishGroup.wishes.length : 0;
      for (var j = 0; j < wishGroupLength; j++) {
        var wish = wishGroup.wishes[j];
        if (wishGroup.id === groupId && wish.id === wishId) {
          wish.name = newName;
        }
      }
    }
    users.updateOne({ _id: idUser }, {
      $set: { wishGroups: wishGroups }
    });
  });
}

function removeWish(idUser, groupId, wishId) {
  var users = mongo.db.collection('user');
  users.findOne({ _id: idUser }, function (err, document) {
    var wishGroups = document.wishGroups;
    for (var i = 0; i < wishGroups.length; i++) {
      var wishGroup = wishGroups[i];
      var wishGroupLength = wishGroup.wishes ? wishGroup.wishes.length : 0;
      for (var j = 0; j < wishGroupLength; j++) {
        var wish = wishGroup.wishes[j];
        if (wishGroup.id === groupId && wish.id === wishId) {
          wishGroup.wishes.splice(j, 1);
          break;
        }
      }
    }
    users.updateOne({ _id: idUser }, {
      $set: { wishGroups: wishGroups }
    });
  });
}

function setProduct(idUser, groupId, wishId, productId) {
  var users = mongo.db.collection('user');
  var request = 'currentBasket.selectedWishes.' + groupId + '.' + wishId + '.product';
  users.updateOne({ _id: idUser }, {
    $set: (0, _defineProperty3.default)({}, request, {
      id: productId,
      quantity: 1
    })
  });
}

function setProductQty(idUser, groupId, wishId, qty) {
  var users = mongo.db.collection('user');
  var request = 'currentBasket.selectedWishes.' + groupId + '.' + wishId + '.product.quantity';
  users.updateOne({ _id: idUser }, {
    $set: (0, _defineProperty3.default)({}, request, parseInt(qty, 10))
  });
}

module.exports = {
  add: addWish,
  select: selectWish,
  rename: renameWish,
  remove: removeWish,
  setProduct: setProduct,
  setProductQty: setProductQty
};

/***/ }),
/* 25 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var LocalStrategy = __webpack_require__(10).Strategy;
var bCrypt = __webpack_require__(8);
var usersManager = __webpack_require__(2);

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
/* 26 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var LocalStrategy = __webpack_require__(10).Strategy;
var bCrypt = __webpack_require__(8);
var usersManager = __webpack_require__(2);
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
/* 27 */
/***/ (function(module, exports) {

module.exports = require("babel-runtime/core-js/promise");

/***/ }),
/* 28 */
/***/ (function(module, exports) {

module.exports = require("mongodb");

/***/ })
/******/ ]);