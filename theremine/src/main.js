import Vue from 'vue';
import App from './components/App';
import router from './router';
import store from './store';

// TODO: move to router
router.beforeEach((to, _, next) => {
  // if (to.meta.auth && auth.user.authenticated !== true) {
  if (to.meta.auth) {
    next('/login');
  } else {
    next();
  }
});

Vue.config.productionTip = false;

/* eslint-disable no-new */
new Vue({
  el: '#app',
  router,
  store,
  template: '<App/>',
  components: { App },
});
