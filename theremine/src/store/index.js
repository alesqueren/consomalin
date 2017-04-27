import Vue from 'vue';
import Vuex from 'vuex';
import replay from '../replay';
import user from './user';
import product from './product';
import schedule from './schedule';
import wishGroup from './wishGroup';
import selection from './selection';
import singleton from './singleton';
import transaction from './transaction';
import sectionWishes from './sectionWishes';

Vue.use(Vuex);

const actions = {
  resetStore: ({ commit }) => {
    commit('resetStore');
  },
};

const mutations = {
  resetStore: (state) => {
    Vue.set(state, 'wishGroup', {});
    Vue.set(state, 'singleton', singleton.defaultState);
    Vue.set(state.product, 'searchs', {});
    Vue.set(state.product, 'details', {});
  },

  setUserData(state, { wishGroups, currentBasket }) {
    return new Promise((resolve) => {
      Vue.set(state, 'wishGroup', wishGroups);
      Vue.set(state, 'selection.basket', currentBasket.selectedWishes);
      Vue.set(state.singleton, 'selectedSlot', currentBasket.currentSlot);
      // Vue.set(state.sectionWishes, 'wid', currentBasket.currentWishId);
      resolve();
    });
  },
};

export default new Vuex.Store({
  strict: true,
  actions,
  mutations,
  plugins: [replay.storePlugin],
  modules: {
    user,
    product,
    schedule,
    wishGroup,
    selection,
    singleton,
    transaction,
    sectionWishes,
  },
});
