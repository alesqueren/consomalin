import Vue from 'vue';
import VueResource from 'vue-resource';

Vue.use(VueResource);

export default {
  user: Vue.resource('/api/users/login'),
  wishGroups: Vue.resource('/api/wishlist'),
};
