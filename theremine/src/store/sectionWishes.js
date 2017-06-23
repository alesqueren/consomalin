import resources from '../resources';
import singleton from './singleton';

function changeHead(array, value) {
  let indexFound = false;
  let beforeIndex = [];
  let afterIndex = [];
  for (let i = 0; i < array.length; i++) {
    const e = array[i];
    if (e === value) {
      indexFound = true;
    }
    if (!indexFound) {
      beforeIndex = beforeIndex.concat(e);
    } else {
      afterIndex = afterIndex.concat(e);
    }
  }
  return afterIndex.concat(beforeIndex);
}

const globalGetters = {
  getCurrent: ({ wid }, getters, rootState, rootGetters) =>
    rootGetters['wishGroup/getWish']({ wid }),

  getOrder: ({ wid }, getters, { wishGroup }, rootGetters) => {
    const wish = rootGetters['wishGroup/getWish']({ wid });
    const gid = wish ? wish.gid : null;

    const groups = rootGetters['selection/getSelectedGroupsIds'];
    const ordGroups = changeHead(groups, gid);

    let res = [];
    for (let i = 0; i < ordGroups.length; i++) {
      const wishes = rootGetters['selection/getSelectedWishesByGroup']({ gid: ordGroups[i] });
      const ordWishes = changeHead(wishes, wid);
      res = res.concat(ordWishes);
    }

    // remove matched wishes
    const matchedW = rootGetters['selection/getMatchedWishesIds'];
    res = res.filter(id => (matchedW.indexOf(id) === -1 && id !== wid));

    return res;
  },

  getLastAdded: ({ wid }, getters, rootState, rootGetters) => {
    const addOrder = rootState.selection.addOrder;
    for (let i = addOrder.length - 1; i >= 0; i--) {
      const isMatched = rootGetters['selection/isMatchedWish'](addOrder[i]);
      if (addOrder[i] !== wid && isMatched) {
        return addOrder[i];
      }
    }
    return null;
  },

  getSameNameWishIds: ({ wid }, getters, rootState, rootGetters) => {
    const res = [];
    if (wid) {
      const wish = rootGetters['wishGroup/getWish']({ wid });
      if (wish) {
        const name = wish.name;
        const wishes = rootGetters['wishGroup/getAllWishes'];

        for (let i = 0; i < wishes.length; i++) {
          if (wishes[i].name === name && wishes[i].id !== wid) {
            if (rootGetters['selection/isMatchedWish'](wishes[i].id)) {
              res.push(wishes[i].id);
            }
          }
        }
      } else {
        return null;
      }
    }
    return res;
  },
};

const actions = {
  processCurrentWish: ({ dispatch }, { currentBasket }) =>
    new Promise((resolve) => {
      if (currentBasket.currentWishId) {
        dispatch('set', currentBasket.currentWishId).then(() => {
          resolve();
        });
      } else {
        dispatch('next', () => null, { root: true }).then(() => {
          resolve();
        });
      }
    }),

  set: ({ state, dispatch, commit, rootGetters }, wid) =>
    new Promise((resolve) => {
      if (wid && rootGetters['wishGroup/getWish']({ wid })) {
        if (wid !== state.wid) {
          commit('set', { wid });
          dispatch('searchProducts');
          resources.currentWish.save({ wid });
        }
      } else {
        commit('set', { wid: null });
        resources.currentWish.delete();
      }
      resolve();
    }),

  next: ({ dispatch, getters }, cb) =>
    new Promise((resolve) => {
      const nextWid = getters.getOrder[0] ? getters.getOrder[0] : null;
      dispatch('set', nextWid);
      if (cb) { cb(); }
      resolve();
    }),

  update: ({ state, dispatch, getters, rootGetters }, mutation) => {
    const currentWid = state.wid;
    if (currentWid) {
      const oldList = getters.getOrder;
      mutation();
      if (!rootGetters['selection/isSelectedWish']({ wid: currentWid })) {
        // currentWish exists and is not selected anymore
        // => find the first selected wish from the list before mutation
        let found = false;
        const selectedWids = rootGetters['selection/getUnmatchedWishes'];
        for (let i = 0; i < oldList.length; i++) {
          const wid = oldList[i];
          if (selectedWids.indexOf(wid) !== -1) {
            dispatch('set', wid);
            found = true;
            break;
          }
        }
        if (!found) {
          // all wishes from the list before mutation are unselected
          dispatch('set', null);
        }
      }
    } else {
      // currentWish does not exist
      // => set the first wish from the list after mutation
      mutation();
      dispatch('set', getters.getOrder[0]);
    }
  },

  searchProducts: ({ dispatch, getters, rootGetters }) => {
    dispatch('product/fetchSearch',
             { name: getters.getCurrent.name },
             { root: true });
    const nextCurrentWish = rootGetters['wishGroup/getWish']({ wid: getters.getOrder[0] });
    if (nextCurrentWish) {
      dispatch('product/fetchSearch',
               { name: nextCurrentWish.name },
               { root: true });
    }
  },

  // debug: ({ getters, rootGetters }) => {
  //   setInterval(() => {
  //     const currName = getters.getCurrent ? getters.getCurrent.name : 'null';
  //     const getName = wid => rootGetters['wishGroup/getWish']({ wid }).name;
  //     const nameList = getters.getOrder.map(getName).join(', ');
  //     // eslint-disable-next-line
  //     console.log(`${currName} [${nameList}]`);
  //   }, 2000);
  // },

};

export default {
  namespaced: true,
  state: {
    wid: null,
  },
  getters: globalGetters,
  actions,
  mutations: singleton.mutations,
};
