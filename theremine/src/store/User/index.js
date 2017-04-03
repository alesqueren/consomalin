import resources from '../../resources';

const actions = {
  login: ({ commit }, { data, success, fail }) => {
    resources.user.login({}, data)
      .then(() => {
        commit('setUser', data.username);
        success();
      }, fail);
  },

  register: ({ commit }, { data, success, fail }) => {
    resources.user.register({}, data)
      .then(() => {
        commit('setUser', data.username);
        success();
      }, fail);
  },

  fetchUser: ({ commit }) =>
    new Promise((resolve, reject) => {
      resources.user.get().then((res) => {
        commit('setUser', res.body.id);
        resolve();
      }, () => {
        commit('setUser', false);
        reject();
      });
    }),

  logout: ({ commit }) => {
    resources.user.logout().then(() => {
      commit('setUser', false);
      commit('resetStore', false);
    });
  },
};

const mutations = {
  setUser: (state, user) => {
    state.user = user;
  },
};

export default {
  state: { user: null },
  actions,
  mutations,
};
