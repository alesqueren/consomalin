import resources from '../resources';

function getFirstUnmatchedWish(currBasket, selection, startIndex) {
  for (let i = startIndex; i < currBasket.length; i++) {
    if (!selection[currBasket[i]]) {
      return currBasket[i];
    }
  }
  for (let i = 0; i < startIndex; i++) {
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

  next({ dispatch, rootGetters, commit, rootState }, wid) {
    if (rootState.selection) {
      const ordSW = rootGetters['selection/getOrdreredSelectedWishes'];
      const oldWishIndex = wid ? ordSW.indexOf(wid) : 0;
      const matchedWishes = rootGetters['selection/getMatchedWishes'];
      const newCurrentWid = getFirstUnmatchedWish(ordSW, matchedWishes, oldWishIndex);
      if (newCurrentWid) {
        const wish = rootGetters['wishGroup/getWish']({ wid: newCurrentWid });
        const gid = wish.gid;
        resources.currentWish.save({}, { gid, wid: wish.id }).then(() => {
          commit('singleton/set', { key: 'currentWishId', value: wish.id }, { root: true });
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
