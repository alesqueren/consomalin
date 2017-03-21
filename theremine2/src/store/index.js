import Vue from 'vue';
import Vuex from 'vuex';
import resources from '../resources';

Vue.use(Vuex);

export default new Vuex.Store({
  state: {
    user: null,
  },
  actions: {
    fetchUser({ commit }) {
      resources.userTmp.get().then((res) => {
        const user = JSON.parse(res.body);
        commit('setUser', user);
      });
    },
  },
  mutations: {
    setUser(state, user) {
      state.user = user;
    },
  },
  strict: true,
});
