import Vue from 'vue';
import VueAnalytics from 'vue-analytics';
import { sync } from 'vuex-router-sync';
import App from './components/App';
import router from './router';
import store from './store';
import config from '../config';

require('./assets/css/layout.css');
require('./assets/css/notepad.css');
require('./assets/css/tooltip.css');
require('./assets/plugins/font-awesome-4.7.0/css/font-awesome.min.css');

Vue.use(VueAnalytics, {
  id: config.analyticsId,
  router,
});

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
