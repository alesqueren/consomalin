import Vue from 'vue';
import Vuex from 'vuex';
import resources from '../resources';

Vue.use(Vuex);
// TODO: add vuex-router-sync
// https://github.com/vuejs/vuex-router-sync

export default new Vuex.Store({
  state: {
    user: null,
    wishGroups: null,
    currentBasket: null,
  },
    // getWish: state => ({ groupId, wishId }) => {
    //   return state.currentBasket.selectedWishes[groupId][wishId];
    // },
  getters: {
    getWish: state => (wishId) => {
      let wishFound = null;
      for (let i = 0; i < state.wishGroups.length; i++) {
        const wishGroup = state.wishGroups[i];
        for (let j = 0; j < wishGroup.wishes.length; j++) {
          const wish = wishGroup.wishes[j];
          if (wish.id === wishId) {
            wishFound = wish;
          }
        }
      }
      return wishFound;
    },
    isSelectedWish: state => ({ groupId, wishId }) => {
      const sw = state.currentBasket.selectedWishes;
      return (sw[groupId] && sw[groupId][wishId]);
    },
    getWishlist: (state) => {
      const wishlist = [];
      // console.log('getters getWishlist je calcule');
      if (state.wishGroups) {
        // console.log('getters getWishlist currentBasket not null');
        const selectedWishes = state.currentBasket.selectedWishes;
        for (let i = 0; i < state.wishGroups.length; i++) {
          const wishgroup = state.wishGroups[i];
          const newWishGroup = {
            id: wishgroup.id,
            name: wishgroup.name,
            wishes: [],
          };
          const wishGroupLength = wishgroup.wishes ? wishgroup.wishes.length : 0;
          for (let j = 0; j < wishGroupLength; j++) {
            const wish = state.wishGroups[i].wishes[j];
            const newWish = {
              id: wish.id,
              name: wish.name,
              groupId: wishgroup.id,
              products: [],
              product: {},
              productInfos: {},
              selected: false,
            };
            const partWishGroupSelect = selectedWishes[newWish.groupId];
            if (partWishGroupSelect && selectedWishes[newWish.groupId][newWish.id]) {
              newWish.selected = true;
            }
            newWishGroup.wishes.push(newWish);
          }
          wishlist.push(newWishGroup);
        }
      }
      // console.log('wishlist :');
      // console.log(wishlist);
      return wishlist;
    },
  },
  actions: {
    updateWishGroupsAndCurrentBasket({ commit, state }) {
      if (!state.wishGroups) {
        resources.wishlist.get(
          {},
          {},
        ).then((response) => {
          const wishGroups = response.data.wishGroups;
          const currentBasket = response.data.currentBasket;
          commit('setWishGroupsAndCurrentBasket', { wishGroups, currentBasket });
        }, () => {
          // console.log('error');
        });
      }
    },
    addWishGroup({ commit }, name) {
      resources.wishgroup.save({}, { name }).then((response) => {
        commit('addWishGroup', { id: response.body, name, wishes: [] });
      }, () => {
        // console.log('error');
      });
    },
    removeWishGroup({ commit }, groupId) {
      resources.wishgroup.delete({ groupid: groupId }, {}).then((response) => {
        commit('removeWishGroup', { groupId });
      }, () => {
        // console.log('error');
      });
      commit('selectWishGroup', { groupId, selected: false });
    },
    addWish({ commit }, { group, name }) {
      resources.wishes.save(
        {
          groupid: group.id,
        }, {
          names: [name],
        }).then((response) => {
          commit('addWish', {
            groupId: group.id,
            id: response.body,
            name,
          });
          commit('selectWish', {
            groupId: group.id,
            wishId: response.body,
            selected: true,
          });
        }, () => {
          // console.log('error');
        });
    },
    removeWish({ commit }, { groupId, wishId }) {
      resources.wish.delete({ groupid: groupId, wishid: wishId }, {}).then(() => {
        commit('removeWish', { wishId });
      }, () => {
        // console.log('error');
      });
      commit('selectWish', { groupId, wishId, selected: false });
    },
    selectWish({ commit }, { groupId, wish, selected }) {
      const wishId = wish.id;

      resources.wish.update(
        {
          groupid: groupId,
          wishid: wishId,
        },
        { selected }).then(() => {
          commit('selectWish', { groupId, wishId, selected });
        }, () => {
        // console.log('error');
        },
      );
    },
    login({ commit }, { data, success, fail }) {
      resources.user.save({}, data)
        .then(() => {
          commit('setUser', data.username);
          success();
        }, fail);
    },
    logout({ commit }) {
      resources.logout.get().then(() => {
        commit('setUser', false);
      });
    },
    fetchUser({ commit }) {
      resources.userTmp.get().then((res) => {
        const user = JSON.parse(res.body);
        commit('setUser', user);
      }, () => {
        commit('setUser', false);
      });
    },
  },
  mutations: {
    addWish(state, { groupId, id, name }) {
      for (let i = 0; i < state.wishGroups.length; i++) {
        const wishgroup = state.wishGroups[i];
        if (wishgroup.id === groupId) {
          wishgroup.wishes.push({
            id,
            name,
          });
        }
      }
    },
    removeWish(state, { wishId }) {
      for (let i = 0; i < state.wishGroups.length; i++) {
        const wishGroup = state.wishGroups[i];
        for (let j = 0; j < wishGroup.wishes.length; j++) {
          const wish = wishGroup.wishes[j];
          if (wish.id === wishId) {
            state.wishGroups[i].wishes.splice(j, 1);
          }
        }
      }
    },
    selectWish(state, { groupId, wishId, selected }) {
      if (!selected) {
        delete state.currentBasket.selectedWishes[groupId][wishId];
      } else {
        if (!state.currentBasket.selectedWishes[groupId]) {
          state.currentBasket.selectedWishes[groupId] = {};
        }
        state.currentBasket.selectedWishes[groupId][wishId] = true;
      }
    },
    addWishGroup(state, { id, name, wishes }) {
      state.wishGroups.push({
        id,
        name,
        wishes,
      });
    },
    removeWishGroup(state, { groupId }) {
      for (let i = 0; i < state.wishGroups.length; i++) {
        const wishgroup = state.wishGroups[i];
        if (wishgroup.id === groupId) {
          state.wishGroups.splice(i, 1);
        }
      }
      if (state.currentBasket.selectedWishes[groupId]) {
        delete state.currentBasket.selectedWishes[groupId];
      }
    },
    selectWishGroup(state, { groupId, selected }) {
      if (!selected) {
        delete state.currentBasket.selectedWishes[groupId];
      } else {
        for (let i = 0; i < state.wishGroups.length; i++) {
          const wishgroup = state.wishGroups[i];
          if (wishgroup.id === groupId) {
            for (let j = 0; j < wishgroup.wishes.length; j++) {
              const wish = wishgroup.wishes[j];
              if (!state.currentBasket.selectedWishes[groupId]) {
                state.currentBasket.selectedWishes[groupId] = {};
              }
              state.currentBasket.selectedWishes[groupId][wish.id] = true;
            }
          }
        }
      }
    },
    setWishGroupsAndCurrentBasket(state, { wishGroups, currentBasket }) {
      // console.log('mutate setWishGroupsAndCurrentBasket');
      state.wishGroups = wishGroups;
      state.currentBasket = currentBasket;
    },
    setUser(state, user) {
      state.user = user;
    },
    setWishlist(state, wishlist) {
      state.wishlist = wishlist;
    },
  },
  strict: true,
});
