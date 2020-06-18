import Vue from 'vue';
import VueResource from 'vue-resource';

Vue.use(VueResource);

export default {
  user: Vue.resource('/api/users/', {}, {
    get: { method: 'GET', url: '/api/users/me' },
    login: { method: 'POST', url: '/api/users/login' },
    register: { method: 'POST', url: '/api/users/register' },
    logout: { method: 'PUT', url: '/api/users/signout' },
  }),
  wishlist: Vue.resource('/api/wishlist'),
  wishgroup: Vue.resource('/api/wishlist/groups{/gid}'),
  wish: Vue.resource('/api/wishlist/groups/{gid}/wishes{/wid}', {}, {
    bulk: { method: 'POST', url: '/api/wishlist/groups/{gid}/wishes/bulk' },
  }),
  wishProduct: Vue.resource('/api/wishlist/groups/{gid}/wishes/{wid}/product', {}, {
    bulk: { method: 'POST', url: '/api/wishlist/groups/{gid}/wishes/{wid}/product/bulk' },
    update: { method: 'PUT', url: '/api/wishlist/groups/{gid}/wishes/{wid}/product' },
    remove: { method: 'DELETE', url: '/api/wishlist/groups/{gid}/wishes/{wid}/product' },
  }),
  currentWish: Vue.resource('/api/wishlist/basket/currentWish'),
  products: Vue.resource('/api/products/{uri}'),
  schedule: Vue.resource('/api/schedule'),
  slot: Vue.resource('/api/wishlist/basket/slot'),
  prepareOrder: Vue.resource('/api/wishlist/basket/prepareOrder'),
  order: Vue.resource('/api/wishlist/basket/order'),
  watcher: Vue.resource('/api/history', {}, {
    get: { method: 'GET', url: '/api/history/sessions{/sid}' },
    add: { method: 'POST', url: '/api/history/add' },
  }),
};
