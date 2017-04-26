import Vue from 'vue';
import { sync } from 'vuex-router-sync';
import App from './components/App';
import router from './router';
import store from './store';

require('./assets/css/layout.css');
require('./assets/css/notepad.css');
require('./assets/css/tooltip.css');
require('./assets/css/rightBar.css');
require('./assets/plugins/font-awesome-4.7.0/css/font-awesome.min.css');

sync(store, router);
Vue.config.productionTip = true;
// Vue.config.performance = true;

/* eslint-disable no-new */
new Vue({
  el: '#app',
  router,
  store,
  template: '<App/>',
  components: { App },
});
