import resources from '../resources';

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

  fetchUserData({ dispatch, commit }) {
    return new Promise((resolve) => {
      resources.wishlist.get({}, {}).then(({ body }) => {
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
          dispatch('basket/detailProductsWithId', { ids: idsWithoutDetail }, { root: true });
        }
        commit('setWishGroupsAndCurrentBasket', { wishGroups, currentBasket }, { root: true });
        resolve();
      });
    });
  },

  logout: ({ commit }) => {
    resources.user.logout().then(() => {
      commit('setUser', false);
      commit('resetStore', false, { root: true });
    });
  },
};

const mutations = {
  setUser: (state, user) => {
    state.user = user;
  },
};

export default {
  namespaced: true,
  state: {
    user: null,
  },
  actions,
  mutations,
};
