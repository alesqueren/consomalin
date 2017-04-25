import resources from '../resources';
import singleton from './singleton';

function reorderFromValue(array, value) {
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

  getList: ({ wid }, getters, { wishGroup }, rootGetters) => {
    const wish = rootGetters['wishGroup/getWish']({ wid });
    const gid = wish ? wish.gid : null;

    const groups = rootGetters['selection/getSelectedGroupsIds'];
    const ordGroups = reorderFromValue(groups, gid);

    let res = [];
    for (let i = 0; i < ordGroups.length; i++) {
      const wishes = rootGetters['selection/getSelectedWishesByGroup']({ gid: ordGroups[i] });
      const ordWishes = reorderFromValue(wishes, wid);
      res = res.concat(ordWishes);
    }

    // removeMatchedWishes
    const matchedW = rootGetters['selection/getMatchedWishesIds'];
    res = res.filter(id => (matchedW.indexOf(id) === -1 && id !== wid));

    return res;
  },
};

const actions = {
  set: ({ state, dispatch, commit, getters, rootGetters }, wid) => {
    if (wid && rootGetters['wishGroup/getWish']({ wid })) {
      if (wid !== state.wid) {
        console.log('set to ' + wid);
        commit('set', { key: 'wid', value: wid });
        dispatch('product/fetchSearch',
                 { name: getters.getCurrent.name },
                 { root: true });
        resources.currentWish.save({}, { wid });
      }
    } else {
      console.log('set to null');
      commit('set', { key: 'wid', value: null });
      resources.currentWish.delete();
    }
  },

  next: ({ dispatch, getters }, cb) => {
    const sectionList = getters.getList;
    if (sectionList[0]) {
      dispatch('set', sectionList[0]);
    } else {
      dispatch('set', null);
    }
    if (cb) { cb(); }
  },

  update: ({ state, dispatch, getters, rootGetters }, mutation) => {
    // TODO:
    //   current:
    //     still selected: do nothing
    //     not selected: oldList[0]
    //   no current:
    //     newList[0]

    let sectionList;
    if (getters.getCurrent) {
      // get next currentWish from old list
      sectionList = getters.getList;
      mutation();
    } else {
      // get next currentWish from the new list
      mutation();
      sectionList = getters.getList;
    }

    for (let i = 0; i < sectionList.length; i++) {
      const wid = sectionList[i];
      const selectedWids = rootGetters['selection/getUnmatchedWishes'];
      if (selectedWids.indexOf(wid) !== -1) {
        dispatch('set', wid);
        return;
      }
    }

    // all wishes are matched or unselected
    dispatch('set', null);
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
