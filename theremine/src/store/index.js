import Vue from 'vue';
import Vuex from 'vuex';
import user from './user';
import product from './product';
import schedule from './schedule';
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
    Vue.set(state, 'wishGroups', {});
    Vue.set(state, 'singleton', {
      selectedSlot: null,
      currentWishId: null,
      actionnedEntity: {},
      activeGroupId: null,
      registering: null,
    });
    Vue.set(state.product, 'searchs', {});
    Vue.set(state.product, 'details', {});
  },

  setUserData(state, { wishGroups, currentBasket }) {
    return new Promise((resolve) => {
      Vue.set(state, 'wishGroup', wishGroups);
      Vue.set(state, 'selection', currentBasket.selectedWishes);
      Vue.set(state.singleton, 'selectedSlot', currentBasket.currentSlot);
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
    user,
    product,
    schedule,
    wishGroup,
    selection,
    singleton,
    currentWish,
    transaction,
  },
});