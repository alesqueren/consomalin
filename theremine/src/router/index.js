import Vue from 'vue';
import Router from 'vue-router';
import Home from '@/components/Home';
import Login from '@/components/User/Login';
import Register from '@/components/User/Register';
import Wishlist from '@/components/Wishlist/Index';
import Section from '@/components/Section/Index';
import Basket from '@/components/Basket/Index';
import Withdraw from '@/components/Withdraw/Index';
import Presentation from '@/components/Presentation/Index';
import Confirmation from '@/components/Confirmation/Index';
import Help from '@/components/Help/Index';
import Ticket from '@/components/Ticket/Index';
import NotFound from '@/components/NotFound';
import store from '../store';
import replay from '../replay';

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
      path: '/connexion',
      component: Login,
    },
    {
      name: 'register',
      path: '/inscription',
      component: Register,
    },
    {
      name: 'wishlist',
      path: '/bloc-note',
      meta: { auth: true },
      component: Wishlist,
    },
    {
      name: 'section',
      path: '/rayons',
      meta: { auth: true },
      component: Section,
    },
    {
      name: 'basket',
      path: '/panier',
      meta: { auth: true },
      component: Basket,
    },
    {
      name: 'withdraw',
      path: '/retrait',
      meta: { auth: true },
      component: Withdraw,
    },
    {
      name: 'confirmation',
      path: '/confirmation',
      meta: { auth: true },
      component: Confirmation,
    },
    {
      name: 'ticket',
      path: '/ticket',
      meta: { auth: true },
      component: Ticket,
    },
    {
      name: 'presentation',
      path: '/presentation',
      meta: { auth: true },
      component: Presentation,
    },
    {
      name: 'help',
      path: '/aide',
      component: Help,
    },
    {
      name: 'replay',
      path: '/replay/:sid',
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
      next('/connexion');
    } else {
      next();
    }
  };

  // jump to page top
  window.$('html,body').scrollTop(0);

  if (to.name === 'replay') {
    replay.init(store, router, to.params.sid);
    return;
  }

  if (store.state.user.username) {
    // user succeded to login
    if (store.state.wishGroup && !store.state.wishGroup[0]) {
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
