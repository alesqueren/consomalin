import Vue from 'vue';
import Vuex from 'vuex';
import replay from '../replay';
import user from './user';
import basket from './basket';
import product from './product';
import schedule from './schedule';
import wishGroup from './wishGroup';
import selection from './selection';
import singleton from './singleton';
import transactions from './transactions';
import sectionWishes from './sectionWishes';

Vue.use(Vuex);

const actions = {
  resetStore: ({ commit }) => {
    commit('resetStore');
  },
  setUserData: ({ dispatch, commit }, data) =>
    (new Promise((resolve) => {
      dispatch('transactions/setAll', data.transactions, { root: true });
      commit('setUserData', data);

      resolve();
    })),
};

const mutations = {
  resetStore: (state) => {
    Vue.set(state, 'wishGroup', {});
    Vue.set(state, 'singleton', singleton.defaultState);
    Vue.set(state.product, 'searchs', {});
    Vue.set(state.product, 'details', {});
  },

  setUserData(state, { wishGroups, currentBasket }) {
    Vue.set(state, 'wishGroup', wishGroups);
    Vue.set(state.selection, 'basket', currentBasket.selectedWishes);
    Vue.set(state.singleton, 'selectedSlot', currentBasket.slot);
  },
};

export default new Vuex.Store({
  strict: true,
  actions,
  mutations,
  plugins: [replay.storePlugin],
  modules: {
    user,
    basket,
    product,
    schedule,
    wishGroup,
    selection,
    singleton,
    transactions,
    sectionWishes,
  },
});
