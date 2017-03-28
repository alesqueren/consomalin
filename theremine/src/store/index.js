import Vue from 'vue';
import Vuex from 'vuex';
import User from './User/index';
import Wishes from './Wishes/index';
import WishGroups from './WishGroups/index';
import Wishlist from './Wishlist/index';
import Basket from './Basket/index';

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
  },
  actions: {
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
    setCurrentWish: (state, groupid, wishid) => {
      state.currentBasket.currentWish = { groupid, wishid };
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
    WishGroups,
    Wishlist,
    Basket,
  },
});
