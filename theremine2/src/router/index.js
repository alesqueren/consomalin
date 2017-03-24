import Vue from 'vue';
import Router from 'vue-router';
import Home from '@/components/Home';
import Login from '@/components/Login';
import Register from '@/components/Register';
import Profile from '@/components/Profile';
import Wishlist from '@/components/Wishlist';
import Section from '@/components/Section';

Vue.use(Router);
Vue.router = new Router({});

export default new Router({
  routes: [
    {
      path: '/',
      component: Home,
    },
    {
      path: '/login',
      component: Login,
    },
    {
      path: '/register',
      component: Register,
    },
    {
      path: '/wishlist',
      component: Wishlist,
    },
    {
      path: '/section',
      component: Section,
    },
    {
      path: '/profile',
      meta: { auth: true },
      component: Profile,
    },
    {
      path: '*',
      redirect: '/',
    },

  ],
});
