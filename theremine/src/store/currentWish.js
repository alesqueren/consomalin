import resources from '../resources';

function getFirstUnmatchedWish(currBasket, selection) {
  for (let i = 0; i < currBasket.length; i++) {
    if (!selection[currBasket[i]]) {
      return currBasket[i];
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

  next({ dispatch, rootGetters, commit, rootState }) {
    if (rootState.selection) {
      const orderdedSelectedWishes = rootGetters['selection/getOrdreredSelectedWishes'];
      const matchedWishes = rootGetters['selection/getMatchedWishes'];
      const newCurrentWid = getFirstUnmatchedWish(orderdedSelectedWishes, matchedWishes);
      if (newCurrentWid) {
        const wish = rootGetters['wishGroup/getWish']({ wid: newCurrentWid });
        const gid = wish.gid;
        const wid = wish.id;
        resources.currentWish.save({}, { gid, wid }).then(() => {
          commit('singleton/set', { key: 'currentWishId', value: wid }, { root: true });
          const currentWish = rootGetters['wishGroup/getWish']({ wid: newCurrentWid });
          if (currentWish.name && !rootState.product.searchs[currentWish.name]) {
            dispatch('product/fetchSearch', { name: currentWish.name }, { root: true });
          }
        });
      } else {
        dispatch('remove');
      }
    }
  },

  remove({ commit }) {
    commit('singleton/unset', { key: 'currentWishId' }, { root: true });
    resources.currentWish.delete();
  },

};

export default {
  namespaced: true,
  actions,
};
