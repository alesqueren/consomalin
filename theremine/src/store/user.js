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

  fetchUserData({ dispatch, rootGetters }) {
    return new Promise((resolve) => {
      resources.wishlist.get().then(({ body }) => {
        const wishGroups = body.wishGroups || {};
        const currentBasket = body.currentBasket;
        if (!currentBasket.selectedWishes) {
          currentBasket.selectedWishes = {};
        }

        dispatch('setUserData', { wishGroups, currentBasket }, { root: true }).then(() => {
          const idsWithoutDetail = rootGetters['selection/getProductsInBasket'];
          if (idsWithoutDetail) {
            dispatch('product/fetchDetails', { ids: idsWithoutDetail }, { root: true }).then(() => {
              resolve();
            });
          }
        });
        if (currentBasket.currentWishId) {
          dispatch('sectionWishes/set', currentBasket.currentWishId, { root: true });
        } else {
          dispatch('sectionWishes/next', () => null, { root: true });
        }
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
