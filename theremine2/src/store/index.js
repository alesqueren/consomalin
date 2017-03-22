import Vue from 'vue';
import Vuex from 'vuex';
import resources from '../resources';

Vue.use(Vuex);
// TODO: add vuex-router-sync
// https://github.com/vuejs/vuex-router-sync

export default new Vuex.Store({
  state: {
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
    login({ commit }, { data, success, fail }) {
      resources.user.save({}, data)
        .then((response) => {
          commit('setUser', data.username);
          success();
        }, fail);
    },
    logout({ commit }) {
      resources.logout.get().then((res) => {
        commit('setUser', false);
      });
    },
    fetchUser({ commit }) {
      resources.userTmp.get().then((res) => {
        const user = JSON.parse(res.body);
        commit('setUser', user);
      }, (res) => {
        commit('setUser', false);
      });
    },
  },
  mutations: {
    setWishGroups(state, wishgroups) {
      state.wishgroups = wishgroups;
    },
    setUser(state, user) {
      state.user = user;
    },
  },
  strict: true,
});
