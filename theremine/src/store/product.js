import Vue from 'vue';
import resources from '../resources';

const actions = {
  fetchDetails: ({ commit }, ids) => {
    const uri = 'details?pids=' + JSON.stringify(ids);
    resources.products.get({ uri }, {}).then(({ body }) => {
      const products = JSON.parse(body);
      Object.keys(products).map((pid) => {
        const detail = products[pid];
        commit('addDetail', { pid, detail });
        return null;
      });
    });
  },

  updateDetail: ({ commit, state }, { pid, detail }) => {
    if (!state.details[pid]) {
      commit('addDetail', { pid, detail });
    }
  },

  fetchSearch: ({ commit, state }, name) => {
    if (!state.searchs[name]) {
      const uri = 'search?s=' + name;
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
        commit('addDetails', products);
      });
    }
  },

};

const mutations = {
  addDetails: (state, products) => {
    Vue.set(state, 'details', products);
  },
  addDetail: (state, { pid, detail }) => {
    Vue.set(state.details, pid, detail);
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
