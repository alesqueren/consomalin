import Vue from 'vue';
import http from 'vue-resource';
import App from './components/App';
import router from './router';
import store from './store';
import auth from './auth';

router.beforeEach((to, _, next) => {
  if (to.meta.auth && auth.user.authenticated !== true) {
    next('/login');
  } else {
    next();
  }
});

const rr = require('@websanova/vue-auth/drivers/router/vue-router.2.x.js');
const rh = require('@websanova/vue-auth/drivers/http/vue-resource.1.x.js');
const ra = require('@websanova/vue-auth/drivers/auth/bearer.js');

Vue.config.productionTip = false;

Vue.use(require('@websanova/vue-auth'), {
  auth,
  http: rh,
  router: rr,
  rolesVar: 'type',
});


/* eslint-disable no-new */
new Vue({
  el: '#app',
  store,
  router,
  http,
  template: '<App/>',
  components: { App },
});
