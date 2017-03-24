const amqp = require('amqplib/callback_api');

const amqpURL = process.env.RABBIT_URI || 'amqp://localhost:5672';
const qname = 'transactions';

// create qname
amqp.connect(amqpURL, (err, conn) => {
  if (err) {
    throw err;
  }
  conn.createChannel((err2, ch) => {
    if (err2) {
      throw err2;
    }
    ch.assertQueue(qname, { durable: true, autoDelete: false });
    setTimeout(() => {
      conn.close();
    }, 10000);
  });
});

module.exports = {
  send: (msg) => {
    amqp.connect(amqpURL, (err, conn) => {
      if (err) {
        console.log('could not send message: ' + err);
        return;
      }
      conn.createChannel((err2, ch) => {
        if (err2) {
          console.log('could not send message: ' + err2);
          conn.close();
          return;
        }
        ch.sendToQueue(qname, new Buffer(msg));
        // cb();
      });

      // close connection after 10s
      // it's enought time to send the message to rabbit.
      setTimeout(() => {
        conn.close();
      }, 10000);
    });
  },
};
