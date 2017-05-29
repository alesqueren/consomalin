import Vue from 'vue';
import resources from '../resources';

const globalGetters = {

  getAllWishes: (state) => {
    let res = [];
    for (let i = 0; i < state.length; i++) {
      res = res.concat(state[i].wishes);
    }
    return res;
  },

  getWish: state => ({ wid }) => {
    for (let i = 0; i < state.length; i++) {
      const group = state[i];
      for (let j = 0; j < group.wishes.length; j++) {
        const wish = group.wishes[j];
        if (wish.id === wid) {
          return {
            id: wish.id,
            name: wish.name,
            gid: group.id,
            gname: group.name,
          };
        }
      }
    }
    return null;
  },

  getGroup: state => ({ gid }) => {
    for (let i = 0; i < state.length; i++) {
      const group = state[i];
      if (group.id === gid) {
        return group;
      }
    }
    return null;
  },
};

const actions = {
  addGroup({ commit }, { name }) {
    return new Promise((resolve) => {
      resources.wishgroup.save({}, { name }).then(({ body }) => {
        commit('addGroup', { gid: body, name, wishes: [] });
        resolve(body);
      });
    });
  },

  renameGroup: ({ commit }, { gid, name }) => {
    resources.wishgroup.update({ gid }, { name });
    commit('renameGroup', { gid, name });
  },

  removeGroup: ({ commit, dispatch }, { gid }) => {
    resources.wishgroup.delete({ gid }).then(() => {
      commit('selection/unselectGroup', { gid }, { root: true });
      commit('removeGroup', { gid });
    });
  },

  addWish({ commit, dispatch }, { gid, name }) {
    resources.wish.bulk({ gid }, { names: [name] }).then((response) => {
      const wid = response.body[0];
      commit('addWish', { gid, wid, name });
      dispatch('selection/selectWish', { wid, selected: true }, { root: true });
    });
  },

  renameWish: ({ commit, rootGetters }, { wid, name }) => {
    const gid = rootGetters['wishGroup/getWish']({ wid }).gid;
    resources.wish.update({ gid, wid }, { name });
    commit('renameWish', { wid, name });
  },

  removeWish: ({ commit, dispatch, getters }, { wid }) => {
    const gid = getters.getWish({ wid }).gid;
    resources.wish.delete({ gid, wid }).then();
    // dispatch('selection/selectWish', { wid, selected: false }, { root: true });
    commit('selectWish/unselectWish', { gid, wid }, { root: true });
    commit('removeWish', { wid });
  },

};

const mutations = {

  addGroup: (state, { gid, name, wishes }) => {
    state.push({ id: gid, name, wishes });
  },

  renameGroup: (state, { gid, name }) => {
    for (let i = 0; i < state.length; i++) {
      if (state[i].id === gid) {
        Vue.set(state[i], 'name', name);
        break;
      }
    }
  },

  removeGroup: (state, { gid }) => {
    for (let i = 0; i < state.length; i++) {
      if (state[i].id === gid) {
        state.splice(i, 1);
        break;
      }
    }
  },

  addWish: (state, { gid, wid, name }) => {
    for (let i = 0; i < state.length; i++) {
      if (state[i].id === gid) {
        state[i].wishes.push({ id: wid, name });
        break;
      }
    }
  },

  renameWish: (state, { wid, name }) => {
    for (let i = 0; i < state.length; i++) {
      for (let j = 0; j < state[i].wishes.length; j++) {
        if (state[i].wishes[j].id === wid) {
          Vue.set(state[i].wishes[j], 'name', name);
          break;
        }
      }
    }
  },

  removeWish: (state, { wid }) => {
    for (let i = 0; i < state.length; i++) {
      for (let j = 0; j < state[i].wishes.length; j++) {
        const wish = state[i].wishes[j];
        if (wish.id === wid) {
          state[i].wishes.splice(j, 1);
          break;
        }
      }
    }
  },

};
export default {
  namespaced: true,
  state: [],
  getters: globalGetters,
  actions,
  mutations,
};
