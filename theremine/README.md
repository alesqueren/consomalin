# Drive Web app

Node app with jade & Mongo

## PREREQUISITE

 - Node 4.\*
 - Mongo 2.2.2

## Installation

```sh
$ npm install
```

You need a Mongo instance (on 27017 port)
Ubuntu

```sh
$ sudo service mongodb start
```

## RUN

```sh
$ npm start
```

"npm start" execute "nodejs ./bin/www"

Default Port : 3000
GO TO http://localhost:3000/

You can set custom port with

```sh
$ PORT=1234 npm start
```

## MISC

Entry point : app.js
Framework : Express
Middleware : Passport for authentication
