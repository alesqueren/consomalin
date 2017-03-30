import resources from '../../resources';

const getters = {
  getWish: (state, commit, rootState) => (wid) => {
    let wishFound = null;
    for (let i = 0; i < rootState.wishGroups.length; i++) {
      const wishGroup = rootState.wishGroups[i];
      for (let j = 0; j < wishGroup.wishes.length; j++) {
        const wish = wishGroup.wishes[j];
        if (wish.id === wid) {
          wishFound = {
            id: wish.id,
            name: wish.name,
            gid: wishGroup.id,
            gname: wishGroup.name,
          };
        }
      }
    }
    return wishFound;
  },
  getCurrentWish(state, commit, rootState) {
    let wishFound = {};
    if (rootState.currentBasket && rootState.currentBasket.currentWish) {
      const currentWish = rootState.currentBasket.currentWish;
      for (let i = 0; i < rootState.wishGroups.length; i++) {
        const wishGroup = rootState.wishGroups[i];
        for (let j = 0; j < wishGroup.wishes.length; j++) {
          const wish = wishGroup.wishes[j];
          if (wish.id === currentWish.wid) {
            wishFound = {
              id: wish.id,
              name: wish.name,
              gid: wishGroup.id,
              gname: wishGroup.name,
            };
          }
        }
      }
    }
    return wishFound;
  },
  isSelectedWish: (state, commit, rootState) => ({ gid, wid }) => {
    const sw = rootState.currentBasket.selectedWishes;
    if (!sw) {
      sw.selectedWishes = {};
    }
    const hasGrpIdP = Object.prototype.hasOwnProperty.call(sw, gid);
    if (hasGrpIdP) {
      return Object.prototype.hasOwnProperty.call(sw[gid], wid);
    }
    return false;
  },
};
const actions = {
  setProduct: ({ commit }, { gid, wid, pid, quantity }) => {
    commit('setProduct', {
      gid,
      wid,
      pid,
      quantity,
    });
    resources.wishProduct.save({ gid, wid }, { pid, quantity });
  },
  addWish: ({ commit }, { group, name }) => {
    const gid = group.id;
    resources.wishes.save({ gid }, { names: [name] }).then((response) => {
      commit('addWish', {
        gid: group.id,
        id: response.body,
        name,
      });
      commit('selectWish', {
        gid: group.id,
        wid: response.body,
        selected: true,
      });
    });
  },
  removeWish: ({ commit }, { gid, wid }) => {
    resources.wish.delete(
      {
        gid,
        wid,
      }, {}).then(() => {
        commit('removeWish', { wid });
      },
    );
    commit('selectWish', { gid, wid, selected: false });
  },
  renameWish: ({ commit }, { gid, wid, name }) => {
    commit('renameWish', { wid, name });
    resources.wish.update(
      {
        gid,
        wid,
      }, { name }).then(() => {
        // commit('renameWish', { wid, name });
      },
    );
  },
  selectWish: ({ commit }, { gid, wid, selected }) => {
    resources.wish.update(
      {
        gid,
        wid,
      },
      { selected }).then(() => {
        commit('selectWish', { gid, wid, selected });
      },
    );
  },
};

export default {
  getters,
  actions,
};
