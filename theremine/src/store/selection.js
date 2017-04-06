import Vue from 'vue';
import resources from '../resources';

const globalGetters = {

  // TODO: rm ?
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

  getMatchedWishesLength: (state) => {
    let length = 0;
    for (let i = 0; i < state.length; i++) {
      const group = state[i];
      for (let j = 0; j < group.wishes.length; j++) {
        if (group.wishes[j].id) {
          length++;
        }
      }
    }
    return length;
  },

  getSelectedGroups: state => Object.keys(state),

  getSelectedWishesByGroup: state => gid => Object.keys(state[gid]),

  isSelectedWish: state => ({ gid, wid }) => {
    const hasGrpIdP = Object.prototype.hasOwnProperty.call(state, gid);
    if (hasGrpIdP) {
      return Object.prototype.hasOwnProperty.call(state[gid], wid);
    }
    return false;
  },

};

const actions = {

  selectGroup: ({ commit }, { gid, selected }) => {
    // TODO: add Promise
    const commitName = selected ? 'selectGroup' : 'unselectGroup';
    commit(commitName, { gid });
    resources.wishgroup.update({ gid }, { selected });
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

  selectGroup: (state, { gid }) => {
    const selectWishes = {};
    for (const i in state) {
      const group = state[i];
      if (group.id === gid) {
        for (const j in group.wishes) {
          const wish = group.wishes[j];
          selectWishes[wish.id] = {};
        }
      }
    }
    Vue.set(state, gid, selectWishes);
  },

  unselectGroup: (state, { gid }) => {
    if (state[gid]) {
      Vue.set(state, gid, null);
      delete state[gid];
    }
  },

  selectWish: (state, { gid, wid, selected }) => {
    // si on deselectionne un wish
    if (!selected) {
      if (state[gid]) {
        Vue.set(state[gid], wid);
        delete state[gid][wid];

        Vue.set(state[gid], 'tmp');
        delete state[gid].tmp;

        // si on a supprimé le dernier wish, on supprime le groupe de l'objet
        if (!Object.keys(state[gid]).length) {
          Vue.set(state, 'tmp');
          delete state.tmp;

          Vue.set(state, gid);
          delete state[gid];
        }
      }
    } else {
      // si le groupe n'existe pas, on le crée
      if (!state[gid]) {
        Vue.set(state, gid, {});
      }
      // dans tous les cas on rajoute le wish a son groupe
      Vue.set(state[gid], wid);
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
