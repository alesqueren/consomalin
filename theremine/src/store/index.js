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
    currentBasket: null,
    searchs: {},
    productInfos: {},
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
            if (!currentBasket.currentWish) {
              currentBasket.currentWish = {};
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
    nextCurrentWish: ({ dispatch, getters, commit, state }) => {
      const currentWish = state.currentBasket.currentWish;
      if (getters.getBasket) {
        const newCurrentWish = getFirstUnmatchedSelectedWish(getters.getBasket);
        if (newCurrentWish && Object.keys(newCurrentWish)) {
          const currentWish2 = getters.getWish(newCurrentWish.wid);
          const gid = currentWish2.gid;
          const wid = currentWish2.id;
          resources.currentWish.save({}, { gid, wid }).then(() => {
            commit('setCurrentWish', { gid: currentWish2.gid, wid: currentWish2.id });
            const currentWishComplete = getters.getCurrentWish;
            if (currentWishComplete.name && !state.searchs[currentWishComplete.name]) {
              dispatch('searchProductsWithName', { name: currentWishComplete.name });
            }
          });
        } else {
          commit('removeCurrentWish');
        }
      }
    },
  },
  resetStore: ({ commit }) => {
    commit('resetStore');
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
        // attention, on voudra toujours que current basket contienne selectedWish
        state.currentBasket = currentBasket;
        resolve();
      });
    },
    setWishlist: (state, wishlist) => {
      state.wishlist = wishlist;
    },
    removeCurrentWish: (state) => {
      Vue.set(state.currentBasket, 'currentWish', {});
    },
    setCurrentWish: (state, { gid, wid }) => {
      Vue.set(state.currentBasket.currentWish, 'gid', gid);
      Vue.set(state.currentBasket.currentWish, 'wid', wid);
      // state.currentBasket.currentWish = { gid, wid };
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
    selectWish: (state, { gid, wid, selected }) => {
      // si on deselectionne un wish

      if (!selected) {
        Vue.set(state.currentBasket.selectedWishes[gid], wid, false);
        delete state.currentBasket.selectedWishes[gid][wid];

        // si on a supprimé le dernier wish, on supprime le groupe de l'objet
        if (!Object.keys(state.currentBasket.selectedWishes[gid]).length) {
          Vue.set(state.currentBasket.selectedWishes, gid, false);
          delete state.currentBasket.selectedWishes[gid];
        }
        // si on selectionne un wish
      } else {
        // si le groupe n'existe pas, on le crée
        if (!state.currentBasket.selectedWishes[gid]) {
          Vue.set(state.currentBasket.selectedWishes, gid, {});
        }
        // dans tous les cas on rajoute le wish a son group
        Vue.set(state.currentBasket.selectedWishes[gid], wid, true);
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

    unselectGroup: (state, { gid }) => {
      if (state.currentBasket.selectedWishes[gid]) {
        Vue.set(state.currentBasket.selectedWishes, gid, false);
        delete state.currentBasket.selectedWishes[gid];
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
