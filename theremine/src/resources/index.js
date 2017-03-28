import Vue from 'vue';
import VueResource from 'vue-resource';

Vue.use(VueResource);

// TODO: use custom actions
// https://github.com/pagekit/vue-resource/blob/develop/docs/resource.md#custom-actions

export default {
  userTmp: Vue.resource('/api/users/me'),
  user: Vue.resource('/api/users/login'),
  userRegister: Vue.resource('/api/users/register'),
  logout: Vue.resource('/api/users/signout'),
  register: Vue.resource('/api/users/register'),
  wishlist: Vue.resource('/api/wishlist'),
  wishgroup: Vue.resource('/api/wishlist/groups{/groupid}'),
  wish: Vue.resource('/api/wishlist/groups/{groupid}/wishes{/wishid}'),
  wishes: Vue.resource('/api/wishlist/groups/{groupid}/wishes/bulk'),
  currentWish: Vue.resource('/api/wishlist/basket/currentWish'),
  product: Vue.resource('/api/products'),
};
