const debug = require('debug')('drive');
const mongoConfig = require('../mongoConfig');
const app = require('../app');
const db = require('../db');

app.set('port', process.env.SERVER_PORT || 3000);

db.connect(mongoConfig.url, function(err) {
  if (err) {
    console.log('Unable to connect to Mongo.')
    process.exit(1)
  } else {
    var port = app.get('port');
    app.listen(port, function() {
        console.log('Listening on port ' + port + '...')
    })
  }
});
