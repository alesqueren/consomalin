import Vue from 'vue';
import Router from 'vue-router';
import Home from '@/components/Home';
import Login from '@/components/Login';
import Register from '@/components/Register';
import Wishlist from '@/components/Wishlist';
import Section from '@/components/Section';

Vue.use(Router);
Vue.router = new Router({});

export default new Router({
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
      // meta: { auth: true },
      component: Wishlist,
    },
    {
      name: 'section',
      path: '/section',
      // meta: { auth: true },
      component: Section,
    },
    {
      name: 'basket',
      path: '/basket',
      // meta: { auth: true },
      // component: Profile,
    },
    {
      name: 'withdraw',
      path: '/withdraw',
      // meta: { auth: true },
      // component: Profile,
    },
    {
      path: '*',
      redirect: '/',
    },
  ],
});
