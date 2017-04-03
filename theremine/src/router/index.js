import Vue from 'vue';
import Router from 'vue-router';
import Home from '@/components/Home';
import Login from '@/components/Login';
import Register from '@/components/Register';
import Wishlist from '@/components/Wishlist/Index';
import Section from '@/components/Section/Index';
import Basket from '@/components/Basket/Index';
import Withdraw from '@/components/Withdraw/Index';
import NotFound from '@/components/NotFound';
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
      component: Withdraw,
    },
    {
      path: '*',
      component: NotFound,
    },
  ],
});

router.beforeEach((to, from, next) => {
  const route = () => {
    if (to.meta.auth) {
      next('/login');
    } else {
      next();
    }
  };

  if (store.state.User.user) {
    if (!store.state.wishGroups) {
      store.dispatch('fetchUserData');
    }
    next();
  } else if (store.state.User.user === false) {
    route();
  } else {
    store.dispatch('fetchUser').then(() => {
      store.dispatch('fetchUserData');
      next();
    }, route);
  }
});

export default router;
