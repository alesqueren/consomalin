import resources from '../resources';

function seekInGroup(wid, wishes, matchedWishes) {
  // find current wish index
  let currI;
  for (let i = 1; i < wishes.length; i++) {
    if (wishes[i].id === wid) {
      currI = i;
      break;
    }
  }

  // loop from current wish index
  for (let i = currI; i < wishes.length + currI; i++) {
    const i2 = i % wishes.length;
    const wish = wishes[i2];
    if (wish.selected && !matchedWishes[wish.id]) {
      return wish;
    }
  }
  return null;
}

function seekInBasket(rootGetters, wishes, matchedWishes) {
  for (let i = 0; i < wishes.length; i++) {
    const wid = wishes[i];
    if (!matchedWishes[wid]) {
      return rootGetters['wishGroup/getWish']({ wid });
    }
  }
  return null;
}

function unsetNextWish(commit, currentWid) {
  commit('singleton/set', {
    key: 'sectionWishes',
    value: {
      previousWid: currentWid,
      currentWid: {},
    },
  }, { root: true });
  resources.currentWish.delete();
}

const actions = {
  set({ rootGetters, commit, rootState, dispatch }, nextWid) {
    return new Promise((resolve) => {
      const currentWid = rootState.singleton.currentWid;
      const nextWish = rootGetters['wishGroup/getWish']({ wid: nextWid });
      commit('singleton/set', {
        key: 'sectionWishes',
        value: {
          previousId: currentWid,
          currentId: nextWish.id,
        },
      }, { root: true });
      resources.currentWish.save({}, {
        gid: nextWish.gid,
        wid: nextWish.id,
      });
      if (!rootState.product.searchs[nextWish.name]) {
        dispatch('product/fetchSearch',
            { name: nextWish.name },
            { root: true });
      }
      resolve();
    });
  },

  next({ dispatch, rootGetters, commit, rootState }, notFoundCb) {
    const wish = rootGetters['wishGroup/getWish']({
      wid: rootState.singleton.currentWid,
    });
    const wid = wish ? wish.id : null;
    const gid = wish ? wish.gid : null;
    const matchedWishes = rootGetters['selection/getMatchedWishes'];
    const wishesInBasket = rootGetters['selection/getOrderedSelectedWishes'];

    let nextWish;
    if (wid) {
      const wishesInGroup = rootGetters['wishGroup/getGroup']({ gid }).wishes;
      nextWish = seekInGroup(wid, wishesInGroup, matchedWishes);
    }
    if (!nextWish) {
      nextWish = seekInBasket(rootGetters, wishesInBasket, matchedWishes);
    }

    if (nextWish) {
      dispatch('set', nextWish);
    } else {
      unsetNextWish(commit, wid);
      if (notFoundCb) {
        notFoundCb();
      }
    }
  },
};

export default {
  namespaced: true,
  actions,
  getters: globalGetters,
};
