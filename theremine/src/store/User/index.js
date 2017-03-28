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
    });
  },
  fetchUser: ({ commit }) => {
    resources.userTmp.get().then((res) => {
      const user = JSON.parse(res.body);
      commit('setUser', user);
    }, () => {
      commit('setUser', false);
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
