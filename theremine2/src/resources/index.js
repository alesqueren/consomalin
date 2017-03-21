import Vue from 'vue';
import VueResource from 'vue-resource';

Vue.use(VueResource);

export default {
  userTmp: Vue.resource('/api/user'),
  user: Vue.resource('/api/users/login'),
  wishlist: Vue.resource('/api/wishlist'),
};
