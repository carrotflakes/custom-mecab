module.exports = {
  entry: __dirname + '/src/js/app.js',
  output: {
    path: __dirname + '/static/js/',
    filename: 'main.js'
  },
  resolve: {
    alias: {
      'vue$': 'vue/dist/vue.common.js'
    }
  }
};
