import Vue from 'vue';
import VueResource from 'vue-resource';
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

Vue.use(VueResource);

const rr = require('@websanova/vue-auth/drivers/router/vue-router.2.x.js');
const rh = require('@websanova/vue-auth/drivers/http/vue-resource.1.x.js');
const ra = require('@websanova/vue-auth/drivers/auth/basic.js');

Vue.config.productionTip = false;

Vue.use(require('@websanova/vue-auth'), {
  auth: ra,
  http: rh,
  router: rr,
  rolesVar: 'type',
});

/* eslint-disable no-new */
new Vue({
  el: '#app',
  router,
  store,
  http: {
    root: '/api',
    headers: {
      Authorization: 'Basic YXBpOnBhc3N3b3Jk',
    },
  },
  template: '<App/>',
  components: { App },
});
