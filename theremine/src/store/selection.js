import Vue from 'vue';
import resources from '../resources';

const globalGetters = {

  getOrdreredSelectedWishes: (state, getters, { wishGroup }) => {
    const wishes = [];
    for (let i = 0; i < wishGroup.length; i++) {
      const group = wishGroup[i];
      for (let j = 0; j < group.wishes.length; j++) {
        const wish = wishGroup[i].wishes[j];
        if (state[group.id] && state[group.id][wish.id]) {
          wishes.push(wish.id);
        }
      }
    }
    return wishes;
  },

  getMatchedWishes: (state, getters, { wishGroup }) => {
    const wishes = {};
    for (let i = 0; i < wishGroup.length; i++) {
      const group = wishGroup[i];
      for (let j = 0; j < group.wishes.length; j++) {
        const wish = wishGroup[i].wishes[j];
        if (state[group.id] && state[group.id][wish.id] && state[group.id][wish.id].pid) {
          const product = state[group.id][wish.id];
          wishes[wish.id] = {
            pid: product.pid,
            quantity: product.quantity,
          };
        }
      }
    }
    return wishes;
  },

  getSelectedGroupsIds: state => Object.keys(state),

  getSelectedWishesByGroup: state => (gid) => {
    if (gid in state) {
      return Object.keys(state[gid]);
    }
    return [];
  },

  getSelectedWishes: state => () => {
    let wishes = 0;
    for (let i = 0; i < state.length; i++) {
      const group = state[i];
      for (let j = 0; j < group.wishes.length; j++) {
        wishes++;
      }
    }
    return wishes;
  },

};

const actions = {

  // TODO: add Promise?
  selectGroup: ({ rootState, commit }, gid) => {
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
    commit('selectGroup', { gid, selectWishes });
    resources.wishgroup.update({ gid }, { selected: true });
  },

  // TODO: add Promise?
  unselectGroup: ({ rootState, commit }, gid) => {
    commit('unselectGroup', { gid });
    resources.wishgroup.update({ gid }, { selected: false });
  },

  selectWish({ commit, rootGetters }, { wid, selected }) {
    return new Promise((resolve) => {
      const gid = rootGetters['wishGroup/getWish'](wid).gid;
      resources.wish.update({ gid, wid }, { selected });
      const commitName = selected ? 'selectWish' : 'unselectWish';
      commit(commitName, { gid, wid, selected });
      resolve();
    });
  },

};

const mutations = {

  setWishProduct: (state, { gid, wid, pid, quantity }) => {
    const entity = state[gid][wid];
    Vue.set(entity, 'pid', pid);
    Vue.set(entity, 'quantity', quantity);
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
