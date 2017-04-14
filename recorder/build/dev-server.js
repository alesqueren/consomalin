var express = require('express')
var webpack = require('webpack');
var webpackConfig = require('./webpack.conf.js');
var compiler = webpack(webpackConfig);

var app = express()

app.use(require("webpack-dev-middleware")(compiler, {
    noInfo: true, publicPath: webpackConfig.output.publicPath
}));

app.use(require("webpack-hot-middleware")(compiler));
