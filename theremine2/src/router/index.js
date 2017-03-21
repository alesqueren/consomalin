import Vue from 'vue';
import Router from 'vue-router';
import Hello from '@/components/Hello';
import Login from '@/components/Login';
import Register from '@/components/Register';
import Profile from '@/components/Profile';

Vue.use(Router);
Vue.router = new Router({});

export default new Router({
  routes: [
    {
      path: '/',
      component: Hello,
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
      path: '/profile',
      meta: { auth: true },
      component: Profile,
    },
    {
      path: '/*',
      redirect: '/',
    },

  ],
});
