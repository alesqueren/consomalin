import Vue from 'vue';
import resources from '../resources';

const globalGetters = {
  getWish: state => (wid) => {
    for (let i = 0; i < state.length; i++) {
      const wishGroup = state[i];
      for (let j = 0; j < wishGroup.wishes.length; j++) {
        const wish = wishGroup.wishes[j];
        if (wish.id === wid) {
          return {
            id: wish.id,
            name: wish.name,
            gid: wishGroup.id,
            gname: wishGroup.name,
          };
        }
      }
    }
    return null;
  },

  getGroup: state => (gid) => {
    for (let i = 0; i < state.length; i++) {
      const wishgroup = state[i];
      if (wishgroup.id === gid) {
        return wishgroup;
      }
    }
    return null;
  },
};

const actions = {
  addGroup({ commit }, name) {
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

  removeGroup: ({ commit }, gid) => {
    resources.wishgroup.delete({ gid }).then(() => {
      commit('selection/unselectGroup', { gid }, { root: true });
      commit('removeGroup', { gid });
    });
  },

  addWish({ commit }, { gid, name }) {
    return new Promise((resolve) => {
      resources.wish.bulk({ gid }, { names: [name] }).then((response) => {
        const wid = response.body[0];
        commit('addWish', { gid, wid, name });
        commit('selection/selectWish', { gid, wid, selected: true }, { root: true });
        resolve(wid);
      });
    });
  },

  renameWish: ({ commit, rootGetters }, { wid, name }) => {
    const gid = rootGetters['wishGroup/getWish'](wid).gid;
    resources.wish.update({ gid, wid }, { name });
    commit('renameWish', { wid, name });
  },

  setWishProduct: ({ rootState, commit }, { gid, wid, pid, quantity }) => {
    commit('selection/setWishProduct', { gid, wid, pid, quantity }, { root: true });
    if (rootState.selection[gid][wid].id) {
      resources.wishProduct.save({ gid, wid }, { pid, quantity });
    } else {
      resources.wishProduct.update({ gid, wid }, { pid, quantity });
    }
  },

  removeWish: ({ commit, getters }, wid) => {
    console.log(wid);
    // console.log(getters.getWish(wid));
    const gid = getters.getWish(wid).gid;
    resources.wish.delete({ gid, wid }).then();
    commit('removeWish', wid);
    commit('selection/selectWish', { gid, wid, selected: false }, { root: true });
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

  removeGroup: (state, gid) => {
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

  removeWish: (state, wid) => {
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
  state: {},
  getters: globalGetters,
  actions,
  mutations,
};
