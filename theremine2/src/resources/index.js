import Vue from 'vue';
import VueResource from 'vue-resource';

Vue.use(VueResource);

export default {
  userTmp: Vue.resource('/api/user'),
  user: Vue.resource('/api/users/login'),
  logout: Vue.resource('/api/users/signout'),
  wishlist: Vue.resource('/api/wishlist'),
  wishgroup: Vue.resource('/api/wishlist/groups{/groupid}'),
  wish: Vue.resource('/api/wishlist/groups/{groupid}/wishes{/wishid}'),
  wishes: Vue.resource('/api/wishlist/groups/{groupid}/wishes/bulk'),
};
