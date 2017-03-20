const mongo = require('./bs/mongo');
const app = require('./app');

app.set('port', process.env.SERVER_PORT || 3000);

mongo.connect(mongo.url, function(err) {
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
