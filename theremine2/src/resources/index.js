import Vue from 'vue';
import VueResource from 'vue-resource';

Vue.use(VueResource);

export default {
  user: Vue.resource('/api/users/login'),
  wishlist: Vue.resource('/api/wishlist'),
};
