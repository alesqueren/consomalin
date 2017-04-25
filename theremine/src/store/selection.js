import Vue from 'vue';
import resources from '../resources';

const globalGetters = {

  getOrderedSelectedWishes: (state, getters, { wishGroup }) => {
    const res = [];
    for (let i = 0; i < wishGroup.length; i++) {
      const group = wishGroup[i];
      for (let j = 0; j < group.wishes.length; j++) {
        const wish = wishGroup[i].wishes[j];
        if (state[group.id] && state[group.id][wish.id]) {
          res.push(wish.id);
        }
      }
    }
    return res;
  },

  getUnmatchedWishes: (state) => {
    const res = [];
    for (const gid in state) {
      for (const wid in state[gid]) {
        if (Object.keys(state[gid][wid]).length === 0) {
          res.push(wid);
        }
      }
    }
    return res;
  },

  getMatchedWishes: (state, getters, { wishGroup }) => {
    const res = {};
    for (let i = 0; i < wishGroup.length; i++) {
      const group = wishGroup[i];
      for (let j = 0; j < group.wishes.length; j++) {
        const wish = wishGroup[i].wishes[j];
        if (state[group.id] && state[group.id][wish.id]) {
          const products = state[group.id][wish.id];
          for (const k in products) {
            if (!res[wish.id]) {
              res[wish.id] = [];
            }
            res[wish.id].push({
              pid: k,
              quantity: parseInt(products[k], 10),
            });
          }
        }
      }
    }
    return res;
  },

  getProductsInBasket: (state, getters, { wishGroup }) => {
    const res = {};
    for (let i = 0; i < wishGroup.length; i++) {
      const group = wishGroup[i];
      for (let j = 0; j < group.wishes.length; j++) {
        const wish = wishGroup[i].wishes[j];
        if (state[group.id] && state[group.id][wish.id]) {
          const products = state[group.id][wish.id];
          for (const k in products) {
            res[k] = true;
          }
        }
      }
    }
    return res;
  },

  getSelectedGroupsIds: (state, getters, { wishGroup }) => {
    const res = [];
    for (let i = 0; i < wishGroup.length; i++) {
      const e = wishGroup[i].id;
      if (Object.keys(state).indexOf(e) !== -1) {
        res.push(e);
      }
    }
    return res;
  },

  getSelectedWishesIds: (state) => {
    const res = [];
    for (const gid in state) {
      for (const wid in state[gid]) {
        res.push(wid);
      }
    }
    return res;
  },

  getMatchedWishesIds: (state) => {
    const res = [];
    for (const gid in state) {
      for (const wid in state[gid]) {
        if (Object.keys(state[gid][wid]).length > 0) {
          res.push(wid);
        }
      }
    }
    return res;
  },

  // TODO: merge with getSelectedWishesByGroup
  // getUnmatchedWishesByGroup: (state, getters, { wishGroup }) => ({ gid }) => {
  //   let ordWishes = [];
  //   for (let i = 0; i < wishGroup.length; i++) {
  //     if (wishGroup[i].id === gid) {
  //       ordWishes = wishGroup[i].wishes;
  //       break;
  //     }
  //   }

  //   const res = [];
  //   for (let i = 0; i < ordWishes.length; i++) {
  //     const wid = ordWishes[i].id;
  //     if (state[gid] && state[gid][wid] &&
  //         !Object.keys(state[gid][wid]).length) {
  //       res.push(wid);
  //     }
  //   }
  //   return res;
  // },

  getSelectedWishesByGroup: (state, getters, { wishGroup }) => ({ gid }) => {
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
      if (state[gid] && state[gid][wid]) {
        res.push(wid);
      }
    }
    return res;
  },

  isSelectedWish: state => ({ wid }) => {
    for (let i = 0; i < state.length; i++) {
      for (let j = 0; j < state[i].length; j++) {
        if (state[i][j] === wid) {
          return true;
        }
      }
    }
    return false;
  },
};

const actions = {

  // TODO: add Promise?
  selectGroup: ({ dispatch, rootState, commit }, { gid }) => {
    const selectWishes = {};
    for (const i in rootState.wishGroup) {
      const wg = rootState.wishGroup;
      if (wg[i].id === gid) {
        for (const j in wg[i].wishes) {
          const wish = wg[i].wishes[j];
          selectWishes[wish.id] = {};
        }
      }
    }
    dispatch('sectionWishes/update',
             () => commit('selectGroup', { gid, selectWishes }),
             { root: true });
    // TODO: rm ?
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

  setWishProducts: ({ commit, rootGetters }, { wid, products }) => {
    const gid = rootGetters['wishGroup/getWish']({ wid }).gid;
    return new Promise((resolve) => {
      commit('setWishProducts', { gid, wid, products });
      resources.wishProduct.bulk({ gid, wid }, { products });
      resolve();
    });
  },

  updateWishProduct: ({ commit, rootGetters }, { wid, pid, quantity }) => {
    const gid = rootGetters['wishGroup/getWish']({ wid }).gid;
    commit('updateWishProduct', { gid, wid, pid, quantity });
    resources.wishProduct.update({ gid, wid }, { pid, quantity });
  },

  removeWishProduct: ({ commit, rootGetters }, { wid, pid }) => {
    const gid = rootGetters['wishGroup/getWish']({ wid }).gid;
    commit('removeWishProduct', { gid, wid, pid });
    resources.wishProduct.remove({ gid, wid }, { pid });
  },

};

const mutations = {

  setWishProducts: (state, { gid, wid, products }) => {
    const entity = state[gid][wid];
    for (const i in products) {
      const product = products[i];
      const pid = product.pid;
      const quantity = product.quantity;
      Vue.set(entity, pid, parseInt(quantity, 10));
    }
  },

  updateWishProduct: (state, { gid, wid, pid, quantity }) => {
    const entity = state[gid][wid];
    Vue.set(entity, pid, parseInt(quantity, 10));
  },

  removeWishProduct: (state, { gid, wid, pid }) => {
    const entity = state[gid][wid];
    Vue.set(entity, pid, null);
    delete state[gid][wid][pid];
    Vue.set(state, 'tmp');
    delete state.tmp;
  },

  selectGroup: (state, { gid, selectWishes }) => {
    Vue.set(state, gid, selectWishes);
  },

  unselectGroup: (state, { gid }) => {
    if (state[gid]) {
      Vue.set(state, gid, null);
      delete state[gid];
      Vue.set(state, 'tmp');
      delete state.tmp;
    }
  },

  selectWish: (state, { gid, wid }) => {
    if (!Object.prototype.hasOwnProperty.call(state, gid)) {
      Vue.set(state, gid, {});
    }
    Vue.set(state[gid], wid, {});
  },

  unselectWish: (state, { gid, wid }) => {
    Vue.set(state[gid], wid);
    delete state[gid][wid];

    Vue.set(state[gid], 'tmp');
    delete state[gid].tmp;

    // delete the group if it doesn't contain selected wishes
    if (!Object.keys(state[gid]).length) {
      Vue.set(state, 'tmp');
      delete state.tmp;

      Vue.set(state, gid);
      delete state[gid];
    }
  },
};

export default {
  namespaced: true,
  state: {},
  getters: globalGetters,
  actions,
  mutations,
};
