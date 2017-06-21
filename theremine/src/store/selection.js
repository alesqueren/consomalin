import Vue from 'vue';
import resources from '../resources';

const globalGetters = {

  getOrderedSelectedWishes: ({ basket }, getters, { wishGroup }) => {
    const res = [];
    for (let i = 0; i < wishGroup.length; i++) {
      const group = wishGroup[i];
      for (let j = 0; j < group.wishes.length; j++) {
        const wish = wishGroup[i].wishes[j];
        if (basket[group.id] && basket[group.id][wish.id]) {
          res.push(wish.id);
        }
      }
    }
    return res;
  },

  getUnmatchedWishes: ({ basket }) => {
    const res = [];
    for (const gid in basket) {
      for (const wid in basket[gid]) {
        if (Object.keys(basket[gid][wid]).length === 0) {
          res.push(wid);
        }
      }
    }
    return res;
  },

  getMatchedWishes: ({ basket }, getters, { wishGroup }) => {
    const res = {};
    for (let i = 0; i < wishGroup.length; i++) {
      const group = wishGroup[i];
      for (let j = 0; j < group.wishes.length; j++) {
        const wish = wishGroup[i].wishes[j];
        if (basket[group.id] && basket[group.id][wish.id]) {
          const products = basket[group.id][wish.id];
          for (const k in products) {
            if (!res[wish.id]) {
              res[wish.id] = [];
            }
            res[wish.id].push({
              pid: products[k].pid,
              quantity: products[k].quantity,
            });
          }
        }
      }
    }
    return res;
  },

  getProductsInBasket: ({ basket }, getters, { wishGroup }) => {
    const res = [];
    for (let i = 0; i < wishGroup.length; i++) {
      const group = wishGroup[i];
      for (let j = 0; j < group.wishes.length; j++) {
        const wish = wishGroup[i].wishes[j];
        if (basket[group.id] && basket[group.id][wish.id]) {
          const products = basket[group.id][wish.id];
          for (const k in products) {
            const pid = products[k].pid;
            res.push(pid);
          }
        }
      }
    }
    return res;
  },

  getSelectedGroupsIds: ({ basket }, getters, { wishGroup }) => {
    const res = [];
    for (let i = 0; i < wishGroup.length; i++) {
      const e = wishGroup[i].id;
      if (Object.keys(basket).indexOf(e) !== -1) {
        res.push(e);
      }
    }
    return res;
  },

  getSelectedWishesIds: ({ basket }) => {
    const res = [];
    for (const gid in basket) {
      for (const wid in basket[gid]) {
        res.push(wid);
      }
    }
    return res;
  },

  getMatchedWishesIds: ({ basket }) => {
    const res = [];
    for (const gid in basket) {
      for (const wid in basket[gid]) {
        if (Object.keys(basket[gid][wid]).length > 0) {
          res.push(wid);
        }
      }
    }
    return res;
  },

  getSelectedWishesByGroup: ({ basket }, getters, { wishGroup }) => ({ gid }) => {
    let ordWishes = [];
    for (let i = 0; i < wishGroup.length; i++) {
      if (wishGroup[i].id === gid) {
        ordWishes = wishGroup[i].wishes;
        break;
      }
    }

    const res = [];
    for (let i = 0; i < ordWishes.length; i++) {
      const wid = ordWishes[i].id;
      if (basket[gid] && basket[gid][wid]) {
        res.push(wid);
      }
    }
    return res;
  },

  getMatchedWishesByGroup: ({ basket }, getters, { wishGroup }) => ({ gid }) => {
    let ordWishes = [];
    for (let i = 0; i < wishGroup.length; i++) {
      if (wishGroup[i].id === gid) {
        ordWishes = wishGroup[i].wishes;
        break;
      }
    }

    const res = [];
    for (let i = 0; i < ordWishes.length; i++) {
      const wid = ordWishes[i].id;
      if (basket[gid] && basket[gid][wid] && basket[gid][wid].length) {
        res.push(wid);
      }
    }
    return res;
  },

  isSelectedWish: ({ basket }) => ({ wid }) => {
    for (const g in basket) {
      for (const w in basket[g]) {
        if (w === wid) {
          return true;
        }
      }
    }
    return false;
  },

  isMatchedWish: ({ basket }) => (wid) => {
    for (const g in basket) {
      for (const w in basket[g]) {
        if (w === wid) {
          return (Object.keys(basket[g][w]).length > 0);
        }
      }
    }
    return false;
  },

  getProduct: ({ basket }) => ({ gid, wid, pid }) => {
    const products = basket[gid][wid];
    for (let i = 0; i < products.length; i++) {
      if (products[i].pid === pid) {
        return products[i];
      }
    }
    return null;
  },

};

const actions = {

  selectGroup: ({ dispatch, rootState, commit }, { gid }) => {
    const selectWishes = {};
    for (const i in rootState.wishGroup) {
      const wg = rootState.wishGroup;
      if (wg[i].id === gid) {
        for (const j in wg[i].wishes) {
          const wish = wg[i].wishes[j];
          selectWishes[wish.id] = [];
        }
      }
    }
    dispatch('sectionWishes/update',
             () => commit('selectGroup', { gid, selectWishes }),
             { root: true });
    resources.wishgroup.update({ gid }, { selected: true });
  },

  unselectGroup: ({ dispatch, commit }, { gid }) => {
    dispatch('sectionWishes/update',
             () => commit('unselectGroup', { gid }),
             { root: true });
    resources.wishgroup.update({ gid }, { selected: false });
  },

  selectWish({ dispatch, commit, rootGetters }, { wid, selected }) {
    return new Promise((resolve) => {
      const gid = rootGetters['wishGroup/getWish']({ wid }).gid;
      resources.wish.update({ gid, wid }, { selected });

      const commitName = selected ? 'selectWish' : 'unselectWish';
      dispatch('sectionWishes/update',
               () => commit(commitName, { gid, wid, selected }),
               { root: true });
      resolve();
    });
  },

  addProduct: ({ commit, rootGetters }, { wid, pid, quantity }) => {
    const gid = rootGetters['wishGroup/getWish']({ wid }).gid;
    return new Promise((resolve) => {
      commit('addProduct', { gid, wid, pid, quantity });
      resources.wishProduct.bulk({ gid, wid }, { products: [{ pid, quantity }] });
      resolve();
    });
  },

  updateProduct: ({ dispatch, commit, rootGetters }, { wid, pid, quantity }) => {
    if (quantity === 0) {
      dispatch('selection/removeProduct', { wid, pid });
    } else {
      const gid = rootGetters['wishGroup/getWish']({ wid }).gid;
      commit('updateProduct', { gid, wid, pid, quantity });
      resources.wishProduct.update({ gid, wid }, { pid, quantity });
    }
  },

  removeProduct: ({ commit, rootGetters, dispatch }, { wid, pid }) => {
    const gid = rootGetters['wishGroup/getWish']({ wid }).gid;
    dispatch('sectionWishes/update',
             () => commit('removeProduct', { gid, wid, pid }),
             { root: true });
    resources.wishProduct.remove({ gid, wid }, { pid });
  },

};

const mutations = {

  selectGroup: ({ basket }, { gid, selectWishes }) => {
    Vue.set(basket, gid, selectWishes);
  },

  unselectGroup: ({ basket }, { gid }) => {
    if (basket[gid]) {
      Vue.set(basket, gid, null);
      delete basket[gid];
      Vue.set(basket, 'tmp');
      delete basket.tmp;
    }
  },

  selectWish: ({ basket }, { gid, wid }) => {
    if (!Object.prototype.hasOwnProperty.call(basket, gid)) {
      Vue.set(basket, gid, {});
    }
    Vue.set(basket[gid], wid, []);
  },

  unselectWish: ({ basket }, { gid, wid }) => {
    Vue.set(basket[gid], wid);
    delete basket[gid][wid];

    Vue.set(basket[gid], 'tmp');
    delete basket[gid].tmp;

    // delete the group if it doesn't contain selected wishes
    if (!Object.keys(basket[gid]).length) {
      Vue.set(basket, 'tmp');
      delete basket.tmp;

      Vue.set(basket, gid);
      delete basket[gid];
    }
  },

  addProduct: (state, { gid, wid, pid, quantity }) => {
    const lastAdd = state.addOrder[state.addOrder.length - 1];
    if (lastAdd !== wid) {
      state.addOrder.push(wid);
    }
    state.basket[gid][wid].push({ pid, quantity });
    // Vue.set(state.basket[gid][wid], pid, quantity);
  },

  updateProduct: ({ basket }, { gid, wid, pid, quantity }) => {
    const products = basket[gid][wid];
    for (let i = 0; i < products.length; i++) {
      if (products[i].pid === pid) {
        Vue.set(products[i], 'quantity', quantity);
        Vue.set(basket, 'tmp');
        delete basket.tmp;
        break;
      }
    }
    // Vue.set(basket[gid][wid], pid, quantity);
  },

  removeProduct: ({ basket }, { gid, wid, pid }) => {
    const products = basket[gid][wid];
    for (let i = 0; i < products.length; i++) {
      if (products[i].pid === pid) {
        products.splice(i, 1);
        break;
      }
    }
  },
};

export default {
  namespaced: true,
  state: {
    addOrder: [],
    basket: {},
  },
  getters: globalGetters,
  actions,
  mutations,
};
