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
