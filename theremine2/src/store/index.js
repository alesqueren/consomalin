import Vue from 'vue';
import Vuex from 'vuex';

Vue.use(Vuex);

export default new Vuex.Store({
  state: {
    count: 0,
    user: null,
  },
  actions: {
    setUser({ commit }, user) {
      commit('setUser', user);
    },
    incrementA({ commit }) {
      commit('increment');
    },
  },
  mutations: {
    setUser(state, user) {
      state.user = user;
    },
    increment(state) {
      state.count += 1;
    },
  },
  strict: true,
});
