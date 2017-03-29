import Vue from 'vue';
import Vuex from 'vuex';
import resources from '../resources';
import User from './User/index';
import Wishes from './Wishes/index';
import WishGroups from './WishGroups/index';
import Wishlist from './Wishlist/index';
import Basket from './Basket/index';

function getFirstUnmatchedSelectedWish(basket) {
  for (let i = 0; i < basket.length; i++) {
    const wish = basket[i];
    if (!wish.product.id) {
      return { groupid: wish.groupId, wishid: wish.id };
    }
  }
  return null;
}

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
    updateWishGroupsAndCurrentBasket({ commit, state }) {
      return new Promise((resolve, reject) => {
        if (!state.wishGroups) {
          resources.wishlist.get(
            {},
            {},
          ).then(({ data }) => {
            const wishGroups = data.wishGroups;
            const currentBasket = data.currentBasket;
            if (!currentBasket.selectedWishes) {
              currentBasket.selectedWishes = {};
            }
            if (!currentBasket.currentWish) {
              currentBasket.currentWish = {};
            }
            commit('setWishGroupsAndCurrentBasket', { wishGroups, currentBasket });
            resolve();
          }, () => {
            reject();
          });
        }
        // console.log('store basket updateblalba');
      });
    },
    processCurrentWish: ({ dispatch, getters, commit, state }) => {
      // console.log('store basket processCurrentWish');
      const currentWish = state.currentBasket.currentWish;
      if (!currentWish.groupid && getters.getBasket) {
        const newCurrentWish = getFirstUnmatchedSelectedWish(getters.getBasket);
        if (newCurrentWish) {
          // console.log(newCurrentWish);
          commit('setCurrentWish', { groupid: newCurrentWish.groupid, wishid: newCurrentWish.wishid });
        }
      }
      if (currentWish) {
        dispatch('searchProductsForWish', { wish: currentWish });
        // this.searchProducts(this.basket.currentWish);
      }

      // resources.currentWish.save({}, { groupid, wishid }).then((response) => {
      // commit('setCurrentWish', { groupid, wishid });
      // }, () => {
        // console.log('error');
      // });
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
      for (let i = 0; i < state.wishGroups.length; i++) {
        const wishgroup = state.wishGroups[i];
        if (wishgroup.id === groupId) {
          state.wishGroups.splice(i, 1);
        }
      }
    },
    setWishGroupsAndCurrentBasket(state, { wishGroups, currentBasket }) {
      // console.log('mutate setWishGroupsAndCurrentBasket');
      return new Promise((resolve) => {
        state.wishGroups = wishGroups;
        // attention, on voudra toujours que current basket contienne selectedWish
        state.currentBasket = currentBasket;
        resolve();
      });
    },
    setWishlist: (state, wishlist) => {
      state.wishlist = wishlist;
    },
    setCurrentWish: (state, { groupid, wishid }) => {
      // console.log('groupid : ' + groupid);
      Vue.set(state.currentBasket.currentWish, 'groupid', groupid);
      Vue.set(state.currentBasket.currentWish, 'wishid', wishid);
      // state.currentBasket.currentWish = { groupid, wishid };
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
    setMatchingProducts: (state, { wish, products }) => {
      for (let i = 0; i < state.wishGroups.length; i++) {
        const wishgroup = state.wishGroups[i];
        if (wishgroup.id === wish.groupId) {
          for (let j = 0; j < wishgroup.length; j++) {
            const tmpWish = wishgroup[j];
            if (tmpWish.id === wish.id) {
              wish.matchingProducts = products;
            }
          }
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
    renameWish: (state, { wishId, name }) => {
      for (let i = 0; i < state.wishGroups.length; i++) {
        const wishGroup = state.wishGroups[i];
        for (let j = 0; j < wishGroup.wishes.length; j++) {
          const wish = wishGroup.wishes[j];
          if (wish.id === wishId) {
            wish.name = name;
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

    selectGroup: (state, { groupId }) => {
      const selectWishes = {};
      for (const i in state.wishGroups) {
        const group = state.wishGroups[i];
        if (group.id === groupId) {
          for (const j in group.wishes) {
            const wish = group.wishes[j];
            selectWishes[wish.id] = {};
          }
        }
      }
      Vue.set(state.currentBasket.selectedWishes, groupId, selectWishes);
    },

    unselectGroup: (state, { groupId }) => {
      if (state.currentBasket.selectedWishes[groupId]) {
        Vue.set(state.currentBasket.selectedWishes, groupId, false);
        delete state.currentBasket.selectedWishes[groupId];
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
