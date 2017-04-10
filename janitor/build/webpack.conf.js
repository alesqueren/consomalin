var webpack = require('webpack')
var nodeExternals = require('webpack-node-externals');

var path = require('path')
function resolve (dir) {
  return path.join(__dirname, '..', dir)
}

module.exports = {
  entry: resolve('src/app.js'),
  output: {
    path: resolve('bin'),
    filename: 'app.bundle.js',
  },
  target: 'node',
  externals: [nodeExternals()],
  // plugins: [
  //   new webpack.HotModuleReplacementPlugin(),
  //   new webpack.NoEmitOnErrorsPlugin()
  // ],
  module: {
    loaders: [{
      test: /\.js$/,
      exclude: /node_modules/,
      loader: 'babel-loader'
    }],
    rules: [
      {
        test: /\.(js|vue)$/,
        loader: 'eslint-loader',
        enforce: "pre",
        include: [resolve('src'), resolve('test')],
      },
      {
        test: /\.js$/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['env']
          }
        },
        include: [resolve('src'), resolve('test')],
      }
    ]
  }
}
