import resources from '../../resources';

const getters = {

  // TODO: rename by getWish
  getByWish: state => (wid) => {
    for (let i = 0; i < state.wishGroups.length; i++) {
      const wishGroup = state.wishGroups[i];
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

  get: state => (gid) => {
    for (let i = 0; i < state.wishGroups.length; i++) {
      const wishgroup = state.wishGroups[i];
      if (wishgroup.id === gid) {
        return wishgroup;
      }
    }
    return null;
  },

  getActiveWishGroup: (state) => {
    const gid = state.activeWishGroup;
    if (gid && state.wishGroups) {
      for (let i = 0; i < state.wishGroups.length; i++) {
        const currGroup = state.wishGroups[i];
        if (currGroup.id === gid) {
          return currGroup;
        }
      }
    }
    return null;
  },

};

const actions = {

  add({ commit }, name) {
    return new Promise((resolve) => {
      resources.wishgroup.save({}, { name }).then((response) => {
        commit('addWishGroup', { id: response.body, name, wishes: [] });
        resolve(response.body);
      });
    });
  },

  setActivation: ({ state, commit }, value) => {
    commit('setWishGroupActivation', value);
  },

  toggleActivation: ({ rootState, commit }, gid) => {
    if (rootState.activeWishGroup === gid) {
      commit('setWishGroupActivation', null);
    } else {
      commit('setWishGroupActivation', gid);
    }
  },

  rename: ({ commit }, { gid, name }) => {
    commit('renameWishGroup', { gid, name });
    resources.wishgroup.update({ gid }, { name });
  },

  remove: ({ commit }, gid) => {
    resources.wishgroup.delete({ gid }).then(() => {
      commit('basket/unselectGroup', { gid }, { root: true });
      commit('removeWishGroup', { gid });
    });
  },
};

const mutations = {
  addWishGroup: (state, { id, name, wishes }) => {
    state.wishGroups.push({ id, name, wishes });
  },

  removeWishGroup: (state, { gid }) => {
    for (let i = 0; i < state.wishGroups.length; i++) {
      const wishgroup = state.wishGroups[i];
      if (wishgroup.id === gid) {
        state.wishGroups.splice(i, 1);
      }
    }
  },
  renameWishGroup: (state, { gid, name }) => {
    for (let i = 0; i < state.wishGroups.length; i++) {
      const wishGroup = state.wishGroups[i];
      if (wishGroup.id === gid) {
        wishGroup.name = name;
        break;
      }
    }
  },

  setWishGroupActivation: (state, gid) => {
    state.activeWishGroup = gid;
  },

  addWish: (state, { gid, id, name }) => {
    for (let i = 0; i < state.wishGroups.length; i++) {
      const wishgroup = state.wishGroups[i];
      if (wishgroup.id === gid) {
        wishgroup.wishes.push({ id, name });
      }
    }
  },

  removeWish: (state, { wid }) => {
    for (let i = 0; i < state.wishGroups.length; i++) {
      const wishGroup = state.wishGroups[i];
      for (let j = 0; j < wishGroup.wishes.length; j++) {
        const wish = wishGroup.wishes[j];
        if (wish.id === wid) {
          state.wishGroups[i].wishes.splice(j, 1);
        }
      }
    }
  },

  renameWish: (state, { wid, name }) => {
    for (let i = 0; i < state.wishGroups.length; i++) {
      const wishGroup = state.wishGroups[i];
      for (let j = 0; j < wishGroup.wishes.length; j++) {
        const wish = wishGroup.wishes[j];
        if (wish.id === wid) {
          wish.name = name;
        }
      }
    }
  },

};

export default {
  namespaced: true,
  state: {
    activeWishGroup: null,
    wishGroups: null,
  },
  getters,
  actions,
  mutations,
};
