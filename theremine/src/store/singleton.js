import Vue from 'vue';

const actions = {
  set({ commit }, { key, value }) {
    commit('set', { key, value });
  },

  unset: ({ state, commit }, { key }) => {
    if (state && state[key]) {
      commit('unset', { key });
    }
  },

  toggle: ({ commit }, { key, value }) => {
    commit('toggle', { key, value });
  },
};

const mutations = {
  set(state, { key, value }) {
    // delete state.key;
    Vue.set(state, key, value);
  },

  unset: (state, { key }) => {
    Vue.set(state, key, {});
  },

  toggle: (state, { key, value }) => {
    const newValue = (state[key] === value) ? null : value;
    Vue.set(state, key, newValue);
  },
};

export default {
  namespaced: true,
  strict: true,
  state: {
    selectedSlot: null,
    currentWishId: null,
    actionnedEntity: {},
    activeGroupId: null,
    registering: null,
  },
  actions,
  mutations,
};
