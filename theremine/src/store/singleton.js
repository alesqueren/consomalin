import Vue from 'vue';

const mutations = {
  set(state, { key, value }) {
    Vue.set(state, key, value);
  },

  toggle: ({ state }, { key, value }) => {
    const newValue = (state[key] === value) ? null : value;
    Vue.set(state, key, newValue);
  },

  unset: ({ state }, key) => {
    Vue.set(state, key, null);
  },
};

export default {
  strict: true,
  state: {
    slotId: null,
    currentWishId: null,
    inlineEditionId: null,
    activeGroupId: null,
  },
  mutations,
};
