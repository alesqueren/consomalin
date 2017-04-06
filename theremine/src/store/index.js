import Vue from 'vue';
import Vuex from 'vuex';
import slot from './slot';
import user from './user';
import product from './product';
import wishGroup from './wishGroup';
import selection from './selection';
import singleton from './singleton';
import currentWish from './currentWish';
import transaction from './transaction';

Vue.use(Vuex);

// TODO: empty file?

const globalGetters = {
  // TODO: rm ?
  getBasket: (state, commit, rootState) => {
    const basket = [];
    const wishGroups = rootState.wishlist.group.wishGroups;
    if (wishGroups && state.selectedWishes) {
      const selectedWishes = state.selectedWishes;
      for (let i = 0; i < wishGroups.length; i++) {
        const wishgroup = wishGroups[i];
        const wishGroupLength = wishgroup.wishes ? wishgroup.wishes.length : 0;
        for (let j = 0; j < wishGroupLength; j++) {
          const wish = wishGroups[i].wishes[j];
          const wishGroupSelect = selectedWishes[wishgroup.id];
          if (wishGroupSelect && selectedWishes[wishgroup.id][wish.id]) {
            const selectedWish = selectedWishes[wishgroup.id][wish.id];
            const productInfos = rootState.productInfos[selectedWish.pid];
            const newWish = {
              id: wish.id,
              name: wish.name,
              gid: wishgroup.id,
              gname: wishgroup.name,
              product: {
                id: selectedWish.pid,
                quantity: selectedWish.quantity,
                infos: productInfos,
              },
            };
            if (state.currentWishId === wish.id) {
              newWish.current = true;
            }
            basket.push(newWish);
          }
        }
      }
    }
    return basket;
  },
};

const actions = {
  resetStore: ({ commit }) => {
    commit('resetStore');
  },
};

const mutations = {
  resetStore: (state) => {
    Vue.set(state, 'wishGroups', null);
    Vue.set(state, 'singleton', null);
    Vue.set(state.product, 'searchs', null);
    Vue.set(state.product, 'details', null);
  },

  setUserData(state, { wishGroups, currentBasket }) {
    return new Promise((resolve) => {
      Vue.set(state, 'wishlist', wishGroups);
      Vue.set(state, 'selection', currentBasket.selectedWishes);
      Vue.set(state.singleton, 'currentWishId', currentBasket.currentWishId);
      resolve();
    });
  },
};


export default new Vuex.Store({
  strict: true,
  getters: globalGetters,
  actions,
  mutations,
  modules: {
    slot,
    user,
    product,
    wishGroup,
    selection,
    singleton,
    currentWish,
    transaction,
  },
});
