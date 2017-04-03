import Vue from 'vue';
import Router from 'vue-router';
import Home from '@/components/Home';
import Login from '@/components/Login';
import Register from '@/components/Register';
import Wishlist from '@/components/Wishlist/Index';
import Section from '@/components/Section/Index';
import Basket from '@/components/Basket/Index';
import store from '../store';

Vue.use(Router);

const router = new Router({
  mode: 'history',
  routes: [
    {
      name: 'home',
      path: '/',
      component: Home,
    },
    {
      name: 'login',
      path: '/login',
      component: Login,
    },
    {
      name: 'register',
      path: '/register',
      component: Register,
    },
    {
      name: 'wishlist',
      path: '/wishlist',
      meta: { auth: true },
      component: Wishlist,
    },
    {
      name: 'section',
      path: '/section',
      meta: { auth: true },
      component: Section,
    },
    {
      name: 'basket',
      path: '/basket',
      meta: { auth: true },
      component: Basket,
    },
    {
      name: 'withdraw',
      path: '/withdraw',
      meta: { auth: true },
      // component: Profile,
    },
    // TODO: 404
    {
      path: '*',
      redirect: '/',
    },
  ],
});

router.beforeEach((to, from, next) => {
  if (to.meta.auth && !store.state.User.user) {
    store.dispatch('fetchUser', () => {
      if (to.meta.auth && !store.state.User.user) {
        next('/login');
      } else {
        next();
      }
    });
  } else {
    next();
  }
});

export default router;
