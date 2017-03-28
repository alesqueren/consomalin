import Vue from 'vue';
import VueResource from 'vue-resource';

Vue.use(VueResource);

export default {
  userTmp: Vue.resource('/api/users/me'),
  user: Vue.resource('/api/users/login'),
  logout: Vue.resource('/api/users/signout'),
  register: Vue.resource('/api/users/register'),
  wishlist: Vue.resource('/api/wishlist'),
  wishgroup: Vue.resource('/api/wishlist/groups{/groupid}'),
  wish: Vue.resource('/api/wishlist/groups/{groupid}/wishes{/wishid}'),
  wishes: Vue.resource('/api/wishlist/groups/{groupid}/wishes/bulk'),
  currentWish: Vue.resource('/api/wishlist/basket/currentWish'),
  product: Vue.resource('/api/products'),
};
