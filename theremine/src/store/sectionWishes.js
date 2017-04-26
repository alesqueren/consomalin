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

    // removeMatchedWishes
    const matchedW = rootGetters['selection/getMatchedWishesIds'];
    res = res.filter(id => (matchedW.indexOf(id) === -1 && id !== wid));

    return res;
  },
};

const actions = {

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

  set: ({ state, dispatch, commit, rootGetters }, wid) => {
    if (wid && rootGetters['wishGroup/getWish']({ wid })) {
      if (wid !== state.wid) {
        commit('set', { wid });
        dispatch('searchProducts');
      }
    } else {
      commit('unset', 'wid');
      resources.currentWish.delete();
    }
  },

  next: ({ dispatch, getters }, cb) => {
    const nextWid = getters.getOrder[0] ? getters.getOrder[0] : null;
    dispatch('set', nextWid);
    if (cb) { cb(); }
  },

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

  debug: ({ getters, rootGetters }) => {
    setInterval(() => {
      const currName = getters.getCurrent ? getters.getCurrent.name : 'null';
      const getName = wid => rootGetters['wishGroup/getWish']({ wid }).name;
      const nameList = getters.getOrder.map(getName).join(', ');
      // eslint-disable-next-line
      console.log(`${currName} [${nameList}]`);
    }, 2000);
  },

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
