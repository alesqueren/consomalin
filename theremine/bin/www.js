const mongoConfig = require('../mongoConfig');
const app = require('../app');
const db = require('../db');

app.set('port', process.env.SERVER_PORT || 3000);

db.connect(mongoConfig.url, (err) => {
  const port = app.get('port');
  if (err) {
    console.log('Unable to connect to Mongo.');
    process.exit(1);
  } else {
    app.listen(port, () => {
      console.log('Listening on port $(port)...');
    });
  }
});
