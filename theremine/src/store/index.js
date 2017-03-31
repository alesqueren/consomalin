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
      // console.log('gid:' + wish.gid + ', wid:' + wish.id);
      return { gid: wish.gid, wid: wish.id };
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
    currentBasket: {
      currentWishId: null,
      selectedWishes: null,
    },
    searchs: {},
    productInfos: {},
    inlineEdition: null,
    activeWishGroup: null,
  },
  // getWish: state => ({ gid, wid }) => {
  //   return state.currentBasket.selectedWishes[gid][wid];
  // },
  getters: {
  },
  actions: {
    updateWishGroupsAndCurrentBasket({ dispatch, commit, state }) {
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
            const idsWithoutDetail = [];
            Object.keys(currentBasket.selectedWishes).map((wishgroupId) => {
              const wishGroup = currentBasket.selectedWishes[wishgroupId];
              Object.keys(wishGroup).map((wishId) => {
                const wish = wishGroup[wishId];
                if (wish.pid) {
                  idsWithoutDetail.push(wish.pid);
                }
                return null;
              });
              return null;
            });
            if (idsWithoutDetail.length) {
              dispatch('detailProductsWithId', { ids: idsWithoutDetail });
            }
            commit('setWishGroupsAndCurrentBasket', { wishGroups, currentBasket });
            resolve();
          }, () => {
            reject();
          });
        } else {
          resolve();
        }
      });
    },

    nextCurrentWish({ dispatch, getters, commit, state }) {
      if (getters.getBasket) {
        const newCurrentWish = getFirstUnmatchedSelectedWish(getters.getBasket);
        if (newCurrentWish) {
          const wish = getters.getWish(newCurrentWish.wid);
          const gid = wish.gid;
          const wid = wish.id;
          resources.currentWish.save({}, { gid, wid }).then(() => {
            commit('setCurrentWish', { gid, wid });
            const currentWish = getters.getWish(wid);
            if (currentWish.name && !state.searchs[currentWish.name]) {
              dispatch('searchProductsWithName', { name: currentWish.name });
            }
          });
        } else {
          commit('removeCurrentWish');
        }
      }
    },

    removeCurrentWish({ commit }) {
      commit('removeCurrentWish');
    },

    setInlineEdition: ({ commit }, id) => {
      commit('setInlineEdition', { id });
    },

    resetStore: ({ commit }) => {
      commit('resetStore');
    },

  },

  mutations: {
    resetStore: (state) => {
      Vue.set(state, 'wishGroups', null);
      Vue.set(state, 'currentBasket', null);
      Vue.set(state, 'searchs', null);
      Vue.set(state, 'productInfos', null);
    },

    addWishGroup: (state, { id, name, wishes }) => {
      state.wishGroups.push({
        id,
        name,
        wishes,
      });
    },

    removeWishGroup: (state, { gid }) => {
      for (let i = 0; i < state.wishGroups.length; i++) {
        const wishgroup = state.wishGroups[i];
        if (wishgroup.id === gid) {
          state.wishGroups.splice(i, 1);
        }
      }
    },

    setWishGroupsAndCurrentBasket(state, { wishGroups, currentBasket }) {
      return new Promise((resolve) => {
        state.wishGroups = wishGroups;
        state.currentBasket = currentBasket;
        resolve();
      });
    },

    setWishlist: (state, wishlist) => {
      state.wishlist = wishlist;
    },

    removeCurrentWish: (state) => {
      Vue.set(state.currentBasket, 'currentWishId', null);
    },

    setCurrentWish: (state, { wid }) => {
      Vue.set(state.currentBasket, 'currentWishId', wid);
    },

    addSearchs: (state, { name, products }) => {
      Vue.set(state.searchs, name, products);
    },

    addProductInfos: (state, { pid, infos }) => {
      Vue.set(state.productInfos, pid, infos);
    },

    addWish: (state, { gid, id, name }) => {
      for (let i = 0; i < state.wishGroups.length; i++) {
        const wishgroup = state.wishGroups[i];
        if (wishgroup.id === gid) {
          wishgroup.wishes.push({
            id,
            name,
          });
        }
      }
    },

    setProduct: (state, { gid, wid, pid, quantity }) => {
      const entity = state.currentBasket.selectedWishes[gid][wid];
      Vue.set(entity, 'pid', pid);
      Vue.set(entity, 'quantity', quantity);
    },

    setMatchingProducts: (state, { wish, products }) => {
      for (let i = 0; i < state.wishGroups.length; i++) {
        const wishgroup = state.wishGroups[i];
        if (wishgroup.id === wish.gid) {
          for (let j = 0; j < wishgroup.wishes.length; j++) {
            const tmpWish = wishgroup.wishes[j];
            if (tmpWish.id === wish.id) {
              Vue.set(state.wishGroups[i].wishes[j], 'matchingProducts', products);
            }
          }
        }
      }
    },

    removeWish: (state, { wid }) => {
      for (let i = 0; i < state.wishGroups.length; i++) {
        const wishGroup = state.wishGroups[i];
        for (let j = 0; j < wishGroup.wishes.length; j++) {
          const wish = wishGroup.wishes[j];
          if (wish.id === wid) {
            state.wishGroups[i].wishes.splice(j, 1);
          }
        }
      }
    },

    renameWish: (state, { wid, name }) => {
      for (let i = 0; i < state.wishGroups.length; i++) {
        const wishGroup = state.wishGroups[i];
        for (let j = 0; j < wishGroup.wishes.length; j++) {
          const wish = wishGroup.wishes[j];
          if (wish.id === wid) {
            wish.name = name;
          }
        }
      }
    },

    renameWishGroup: (state, { gid, name }) => {
      for (let i = 0; i < state.wishGroups.length; i++) {
        const wishGroup = state.wishGroups[i];
        if (wishGroup.id === gid) {
          wishGroup.name = name;
          break;
        }
      }
    },

    selectWish: (state, { gid, wid, selected }) => {
      const selectedWishes = state.currentBasket.selectedWishes;
      // si on deselectionne un wish
      if (!selected) {
        if (selectedWishes[gid]) {
          Vue.set(selectedWishes[gid], wid, null);
          delete selectedWishes[gid][wid];

          Vue.set(selectedWishes[gid], 'tmp', {});
          delete selectedWishes[gid].tmp;

          // si on a supprimé le dernier wish, on supprime le groupe de l'objet
          if (!Object.keys(selectedWishes[gid]).length) {
            Vue.set(selectedWishes, 'tmp', {});
            delete selectedWishes.tmp;

            Vue.set(selectedWishes, gid, null);
            delete selectedWishes[gid];
          }
        }
        // si on selectionne un wish
      } else {
        // si le groupe n'existe pas, on le crée
        if (!selectedWishes[gid]) {
          Vue.set(selectedWishes, gid, {});
        }
        // dans tous les cas on rajoute le wish a son group
        Vue.set(selectedWishes[gid], wid, {});
      }
    },

    selectGroup: (state, { gid }) => {
      const selectWishes = {};
      for (const i in state.wishGroups) {
        const group = state.wishGroups[i];
        if (group.id === gid) {
          for (const j in group.wishes) {
            const wish = group.wishes[j];
            selectWishes[wish.id] = {};
          }
        }
      }
      Vue.set(state.currentBasket.selectedWishes, gid, selectWishes);
    },

    setWishGroupActivation: (state, gid) => {
      state.activeWishGroup = gid;
    },

    unselectGroup: (state, { gid }) => {
      if (state.currentBasket.selectedWishes[gid]) {
        Vue.set(state.currentBasket.selectedWishes, gid, null);
        delete state.currentBasket.selectedWishes[gid];
      }
    },

    setInlineEdition: (state, { id }) => {
      state.inlineEdition = id;
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
