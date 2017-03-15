const amqp = require("amqplib/callback_api");
const amqpURL = process.env.RABBIT_URI || 'amqp://localhost:5672';
const qname = "transactions";

 // create qname
amqp.connect(amqpURL, function(err, conn) {
    if (err) {
     throw err;
    }
    conn.createChannel(function(err, ch) {
        if (err) {
            throw err;
        }
        ch.assertQueue(qname, {durable: true, autoDelete: false});
        setTimeout(function() {
            conn.close();
        }, 10000);
    });
});

module.exports = {
    send: function (msg, cb) {
     amqp.connect(amqpURL, function(err, conn) {
         if (err) {
             console.log("could not send message: " + err);
             return;
         }
         conn.createChannel(function(err, ch) {
             if (err) {
                 console.log("could not send message: " + err);
                 conn.close();
                 return;
             }
             ch.sendToQueue(qname, new Buffer(msg));
             // cb();
         });

         // close connection after 10s
         // it's enought time to send the message to rabbit.
         setTimeout(function() {
             conn.close();
         }, 10000);
     });
    }
};