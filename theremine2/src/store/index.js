import Vue from 'vue';
import Vuex from 'vuex';

Vue.use(Vuex);

export default new Vuex.Store({
  state: {
    count: 0,
  },
  actions: {
    incrementA({ commit }) {
      commit('increment');
    },
  },
  mutations: {
    increment(state) {
      state.count += 1;
    },
  },
  strict: true,
});
