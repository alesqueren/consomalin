import Vue from 'vue';
import Vuex from 'vuex';
import resources from '../resources';
import User from './User/index';
import Wishes from './Wishes/index';

Vue.use(Vuex);
// TODO: add vuex-router-sync
// https://github.com/vuejs/vuex-router-sync

export default new Vuex.Store({
  state: {
    wishGroups: null,
    currentBasket: null,
  },
    // getWish: state => ({ groupId, wishId }) => {
    //   return state.currentBasket.selectedWishes[groupId][wishId];
    // },
  getters: {
    isSelectedWishGroup: state => ({ groupId }) => {
      const selectedWishes = state.currentBasket.selectedWishes;
      return (selectedWishes && selectedWishes[groupId]);
    },
    getWishlist: (state) => {
      const wishlist = [];
      if (state.wishGroups) {
        const selectedWishes = state.currentBasket.selectedWishes;
        for (let i = 0; i < state.wishGroups.length; i++) {
          const wishgroup = state.wishGroups[i];
          const newWishGroup = {
            id: wishgroup.id,
            name: wishgroup.name,
            wishes: [],
            selected: false,
          };
          const wishGroupLength = wishgroup.wishes ? wishgroup.wishes.length : 0;
          for (let j = 0; j < wishGroupLength; j++) {
            const wish = state.wishGroups[i].wishes[j];
            const newWish = {
              id: wish.id,
              name: wish.name,
              groupId: wishgroup.id,
              selected: false,
            };
            if (selectedWishes) {
              const wishGroupSelect = selectedWishes[newWish.groupId];
              if (wishGroupSelect && wishGroupSelect[newWish.id]) {
                newWish.selected = true;
                newWishGroup.selected = true;
              }
            }
            newWishGroup.wishes.push(newWish);
          }
          wishlist.push(newWishGroup);
        }
      }
      return wishlist;
    },
    getBasket: (state) => {
      const basket = [];
      if (state.wishGroups && state.currentBasket.selectedWishes) {
        const selectedWishes = state.currentBasket.selectedWishes;
        for (let i = 0; i < state.wishGroups.length; i++) {
          const wishgroup = state.wishGroups[i];
          const wishGroupLength = wishgroup.wishes ? wishgroup.wishes.length : 0;
          for (let j = 0; j < wishGroupLength; j++) {
            const wish = state.wishGroups[i].wishes[j];
            const wishGroupSelect = selectedWishes[wishgroup.id];
            if (wishGroupSelect && selectedWishes[wishgroup.id][wish.id]) {
              const selectedWish = selectedWishes[wishgroup.id][wish.id];
              const newWish = {
                id: wish.id,
                name: wish.name,
                groupId: wishgroup.id,
                matchingProducts: [],
                product: {
                  id: selectedWish.pid,
                  quantity: selectedWish.quantity,
                },
                selected: true,
              };
              basket.push(newWish);
            }
          }
          // if (wishes.length) {
          //   const newWishGroup = {
          //     id: wishgroup.id,
          //     name: wishgroup.name,
          //     wishes,
          //     selected: true,
          //   };
          //   basket.push(newWishGroup);
          // }
        }
      }
      // console.log('basket');
      // console.log(basket);
      return basket;
    },
  },
  actions: {
    updateWishGroupsAndCurrentBasket: ({ commit, state }) => {
      if (!state.wishGroups) {
        resources.wishlist.get(
          {},
          {},
        ).then((response) => {
          const data = JSON.parse(response.data);
          const wishGroups = data.wishGroups;
          const currentBasket = data.currentBasket;
          if (!currentBasket.selectedWishes) {
            currentBasket.selectedWishes = {};
          }
          commit('setWishGroupsAndCurrentBasket', { wishGroups, currentBasket });
        }, () => {
          // console.log('error');
        });
      }
    },
    addWishGroup: ({ commit }, name) => {
      resources.wishgroup.save({}, { name }).then((response) => {
        commit('addWishGroup', { id: response.body, name, wishes: [] });
      }, () => {
        // console.log('error');
      });
    },
    removeWishGroup: ({ commit }, groupId) => {
      resources.wishgroup.delete({ groupid: groupId }, {}).then(() => {
        commit('removeWishGroup', { groupId });
      }, () => {
        // console.log('error');
      });
      commit('selectWishGroup', { groupId, selected: false });
    },
  },
  mutations: {
    addWishGroup: (state, { id, name, wishes }) => {
      state.wishGroups.push({
        id,
        name,
        wishes,
      });
    },
    removeWishGroup: (state, { groupId }) => {
      if (state.currentBasket.selectedWishes[groupId]) {
        Vue.set(state.currentBasket.selectedWishes, groupId, false);
        delete state.currentBasket.selectedWishes[groupId];
      }
      for (let i = 0; i < state.wishGroups.length; i++) {
        const wishgroup = state.wishGroups[i];
        if (wishgroup.id === groupId) {
          state.wishGroups.splice(i, 1);
        }
      }
    },
    setWishGroupsAndCurrentBasket: (state, { wishGroups, currentBasket }) => {
      // console.log('mutate setWishGroupsAndCurrentBasket');
      state.wishGroups = wishGroups;
      // attention, on voudra toujours que current basket contienne selectedWish
      state.currentBasket = currentBasket;
    },
    setWishlist: (state, wishlist) => {
      state.wishlist = wishlist;
    },
    addWish: (state, { groupId, id, name }) => {
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
    removeWish: (state, { wishId }) => {
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
    selectWish: (state, { groupId, wishId, selected }) => {
      // si on deselectionne un wish

      if (!selected) {
        Vue.set(state.currentBasket.selectedWishes[groupId], wishId, false);
        delete state.currentBasket.selectedWishes[groupId][wishId];

        // si on a supprimé le dernier wish, on supprime le groupe de l'objet
        if (!Object.keys(state.currentBasket.selectedWishes[groupId]).length) {
          Vue.set(state.currentBasket.selectedWishes, groupId, false);
          delete state.currentBasket.selectedWishes[groupId];
        }
        // si on selectionne un wish
      } else {
        // si le groupe n'existe pas, on le crée
        if (!state.currentBasket.selectedWishes[groupId]) {
          Vue.set(state.currentBasket.selectedWishes, groupId, {});
        }
        // dans tous les cas on rajoute le wish a son group
        Vue.set(state.currentBasket.selectedWishes[groupId], wishId, true);
      }
    },
  },
  strict: true,
  modules: {
    User,
    Wishes,
  },
});
