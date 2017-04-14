import Vue from 'vue';
import resources from '../resources';

const actions = {
  fetchDetails: ({ commit }, { ids }) => {
    const uri = 'details?pids=' + JSON.stringify(ids);
    resources.products.get({ uri }, {}).then(({ body }) => {
      const products = JSON.parse(body);
      commit('addDetails', { products });
    });
  },

  fetchSearch: ({ commit, state }, { name }) => {
    if (!state.searchs[name]) {
      const uri = 'search?s="' + name + '"';
      resources.products.get({ uri }, {}).then(({ body }) => {
        const products = JSON.parse(body);
        // todo: see array v-for
        commit('addSearch', {
          name,
          products: Object.keys(products).reduce((acc, cur, i) => {
            acc[i] = cur;
            return acc;
          }, {}),
        });
        commit('addDetails', { products });
      });
    }
  },

};

const mutations = {
  addDetails: (state, { products }) => {
    Object.keys(products).map((pid) => {
      const detail = products[pid];
      // commit('addDetail', { pid, detail });
      Vue.set(state.details, pid, detail);
      return null;
    });
  },

  addSearch: (state, { name, products }) => {
    Vue.set(state.searchs, name, products);
  },

};

export default {
  namespaced: true,
  state: {
    searchs: {},
    details: {},
  },
  actions,
  mutations,
};