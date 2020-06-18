import Vue from 'vue';

// TODO: move to other store files
//       and import mutations ?
const defaultState = {
  selectedSlot: null,
  action: { name: '', value: {} },
  activeGroupId: null,
  registering: null,
  flash: { content: '', type: '' },
};

const actions = {
  set({ commit }, object) {
    commit('set', object);
  },

  unset: ({ state, commit }, key) => {
    if (key && Object.keys(state[key]).length) {
      commit('unset', key);
    }
  },
};

const mutations = {
  set: (state, object) => {
    Object.keys(object).map((key) => {
      Vue.set(state, key, object[key]);
      return null;
    });
  },

  unset: (state, key) => {
    Vue.set(state, key, {});
  },
};

export default {
  namespaced: true,
  strict: true,
  state: defaultState,
  actions,
  mutations,
  defaultState,
};
