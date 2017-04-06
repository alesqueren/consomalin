import resources from '../resources';

function getFirstUnmatchedSelectedWish(currBasket) {
  for (let i = 0; i < currBasket.length; i++) {
    const wish = currBasket[i];
    if (!wish.product.id) {
      return { gid: wish.gid, wid: wish.id };
    }
  }
  return null;
}

const actions = {
  set({ commit }, { wid }) {
    return new Promise((resolve) => {
      resources.currentWish.save({}, { wid }).then(() => {
        commit('singleton/set', { key: 'currentWishId', value: wid }, { root: true });
        resolve();
      });
    });
  },

  next({ dispatch, rootGetters, commit, state }) {
    if (rootGetters.getBasket) {
      const newCurrentWish = getFirstUnmatchedSelectedWish(rootGetters.getBasket);
      if (newCurrentWish) {
        const wish = rootGetters['wishGroup/getWish'](newCurrentWish.wid);
        const gid = wish.gid;
        const wid = wish.id;
        resources.currentWish.save({}, { gid, wid }).then(() => {
          commit('singleton/set', { key: 'currentWishId', value: wid }, { root: true });
          const currentWish = rootGetters['wishGroup/getWish'](wid);
          if (currentWish.name && !state.searchs[currentWish.name]) {
            dispatch('product/fetchSearch', currentWish.name, { root: true });
          }
        });
      } else {
        dispatch('remove');
      }
    }
  },

  remove({ commit }) {
    commit('singleton/unset', 'currentWishId', { root: true });
    resources.currentWish.delete();
  },

};

export default {
  namespaced: true,
  actions,
};
