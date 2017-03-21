import Vue from 'vue';
import Vuex from 'vuex';
import resources from '../resources';

Vue.use(Vuex);

export default new Vuex.Store({
  state: {
    count: 0,
    user: null,
    wishgroups: null,
  },
  actions: {
    setWishGroups({ commit }) {
      resources.wishGroups.get(
        {},
        {},
      ).then((response) => {
        console.log(response.data);
        const wishGroups = response.data.wishGroups;
        const selectedWishes = response.data.selectedWishes;
        const wishGroupsLength = wishGroups.length;
        for (let i = 0; i < wishGroupsLength; i++) {
          const wishGroup = wishGroups[i];
          const wishGroupLength = wishGroup.wishes ? wishGroup.wishes.length : 0;
          for (let j = 0; j < wishGroupLength; j++) {
            const wish = wishGroups[i].wishes[j];
            wish.products = [];
            wish.product = {};
            wish.productInfos = {};
            wish.selected = false;
            if (selectedWishes[wishGroup.id] && selectedWishes[wishGroup.id][wish.id]) {
              wish.selected = true;
            }
          }
        }
        commit('setWishGroups', wishGroups);
      }, () => {
        console.log('error');
      });
    },
    setUser({ commit }, user) {
      commit('setUser', user);
    },
    incrementA({ commit }) {
      commit('increment');
    },
  },
  mutations: {
    setWishGroups(state, wishgroups) {
      state.wishgroups = wishgroups;
    },
    setUser(state, user) {
      state.user = user;
    },
    increment(state) {
      state.count += 1;
    },
  },
  strict: true,
});
