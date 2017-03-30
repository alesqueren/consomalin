import resources from '../../resources';

const actions = {
  login: ({ commit }, { data, success, fail }) => {
    resources.user.save({}, data)
      .then(() => {
        commit('setUser', data.username);
        success();
      }, fail);
  },
  register: ({ commit }, { data, success, fail }) => {
    resources.userRegister.save({}, data)
      .then(() => {
        commit('setUser', data.username);
        success();
      }, fail);
  },
  logout: ({ commit }) => {
    resources.logout.get().then(() => {
      commit('setUser', false);
      commit('resetStore', false);
    });
  },
  fetchUser: ({ commit }, callback) => {
    resources.userTmp.get().then((res) => {
      commit('setUser', res.body.id);
      if (callback) {
        callback();
      }
    }, () => {
      commit('setUser', false);
      if (callback) {
        callback();
      }
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
