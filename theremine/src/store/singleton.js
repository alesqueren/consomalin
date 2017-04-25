import Vue from 'vue';

const actions = {
  set({ commit }, { key, value }) {
    commit('set', { key, value });
  },

  unset: ({ state, commit }, { key }) => {
    if (Object.keys(state[key]).length) {
      commit('unset', { key });
    }
  },

  toggle: ({ commit }, { key, value }) => {
    commit('toggle', { key, value });
  },
};

const mutations = {
  set: (state, { key, value }) => {
    Vue.set(state, key, value);
  },

  merge: (state, object) => {
    Object.keys(object).map((key) => {
      Vue.set(state, key, object[key]);
      return null;
    });
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
    currentWid: null,
    actionnedEntity: {},
    activeGroupId: null,
    registering: null,
    previousWid: null,
  },
  actions,
  mutations,
};
