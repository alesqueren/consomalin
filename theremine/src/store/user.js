import resources from '../resources';

const actions = {
  login: ({ commit }, { data, success, fail }) => {
    resources.user.login({}, data).then(() => {
      commit('set', data.username);
      success();
    }, fail);
  },

  register: ({ commit }, { data, success, fail }) => {
    resources.user.register({}, data).then(() => {
      commit('set', data.username);
      success();
    }, fail);
  },

  fetchUser: ({ commit }) =>
    new Promise((resolve, reject) => {
      resources.user.get().then(({ body }) => {
        commit('set', body.id);
        resolve();
      }, () => {
        commit('set', false);
        reject();
      });
    }),

  fetchUserData({ dispatch, commit }) {
    return new Promise((resolve) => {
      resources.wishlist.get().then(({ body }) => {
        const wishGroups = body.wishGroups;
        const currentBasket = body.currentBasket;
        if (!currentBasket.selectedWishes) {
          currentBasket.selectedWishes = {};
        }
        const idsWithoutDetail = [];
        Object.keys(currentBasket.selectedWishes).map((wishgroupId) => {
          const wishGroup = currentBasket.selectedWishes[wishgroupId];
          Object.keys(wishGroup).map((wishId) => {
            const wish = wishGroup[wishId];
            if (wish.pid) {
              idsWithoutDetail.push(wish.pid);
            }
            return null;
          });
          return null;
        });
        if (idsWithoutDetail.length) {
          dispatch('product/fetchDetails', idsWithoutDetail, { root: true });
        }
        commit('setUserData', { wishGroups, currentBasket }, { root: true });
        resolve();
      });
    });
  },

  logout: ({ commit }) => {
    resources.user.logout().then(() => {
      commit('set', false);
      commit('resetStore', null, { root: true });
    });
  },
};

const mutations = {
  set: (state, user) => {
    state.username = user;
  },
};

export default {
  namespaced: true,
  state: {
    username: null,
  },
  actions,
  mutations,
};
