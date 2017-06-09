import Vue from 'vue';
import resources from '../resources';

const globalGetters = {
  getWishesAssociate: (state, getters, rootState, rootGetters) => ({ pid }) => {
    const res = [];
    const matchedWishes = rootGetters['selection/getMatchedWishes'];
    if (matchedWishes) {
      Object.keys(matchedWishes).map((wid, index) => {
        const wish = matchedWishes[wid];
        for (let i = 0; i < wish.length; i++) {
          if (wish[i].pid === pid) {
            res.push(wid);
          }
        }
        return true;
      });
    }
    return res;
  },
  getTotalQuantity: (state, getters, rootState, rootGetters) => ({ pid }) => {
    let res = 0;
    const matchedWishes = rootGetters['selection/getMatchedWishes'];
    if (matchedWishes) {
      Object.keys(matchedWishes).map((wid, index) => {
        const wish = matchedWishes[wid];
        for (let i = 0; i < wish.length; i++) {
          if (wish[i].pid === pid) {
            res += wish[i].quantity;
          }
        }
        return true;
      });
    }
    return res;
  },
};

const actions = {
  fetchDetails: ({ commit }, { ids }) =>
    new Promise((resolve) => {
      if (ids.length) {
        const uri = 'details?pids=' + JSON.stringify(ids);
        resources.products.get({ uri }, {}).then(({ body }) => {
          const products = JSON.parse(body);
          commit('addDetails', { products });
          resolve();
        });
      }
    }),

  fetchSearch: ({ commit, state }, { name }) => {
    if (!state.searchs[name]) {
      const uri = 'search?s="' + name + '"';
      resources.products.get({ uri }, {}).then(({ body }) => {
        const products = JSON.parse(body);
        commit('addDetails', { products });
        commit('addSearch', {
          name,
          products: Object.keys(products).reduce((acc, cur, i) => {
            acc[i] = cur;
            return acc;
          }, {}),
        });
      });
    }
  },

  addProductInMultipleWish: ({ commit, state }, { pid }) => {
    if (!state.productInMultipleWish[pid]) {
      commit('addProductInMultipleWish', { pid });
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

  addProductInMultipleWish: (state, { pid }) => {
    Vue.set(state.productInMultipleWish, pid, true);
    Vue.set(state.productInMultipleWish, 'tmp');
    delete state.productInMultipleWish.tmp;
  },
};

export default {
  namespaced: true,
  state: {
    searchs: {},
    details: {},
    productInMultipleWish: {},
  },
  getters: globalGetters,
  actions,
  mutations,
};
