import Vue from 'vue';
import Router from 'vue-router';
import Home from '@/components/Home';
import Login from '@/components/Login';
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
  const routeOrRedirect = () => {
    if (to.meta.auth) {
      next('/login');
    } else {
      next();
    }
  };

  if (store.state.user.username) {
    // user succeded to login
    if (!store.state.wishGroup[0]) {
      store.dispatch('user/fetchUserData').then(next);
    } else {
      next();
    }
  } else if (store.state.user === false) {
    // user failed to login
    routeOrRedirect();
  } else {
    // we don't know
    // next();
    // store.dispatch('user/fetchUser');
    // store.dispatch('user/fetchUserData');
    store.dispatch('user/fetchUser').then(() => {
      store.dispatch('user/fetchUserData').then(next);
    }, routeOrRedirect);
  }
});

export default router;
