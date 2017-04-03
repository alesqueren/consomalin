import resources from '../../resources';

const getters = {

  getWish: (state, commit, rootState) => (wid) => {
    for (let i = 0; i < rootState.wishGroups.length; i++) {
      const wishGroup = rootState.wishGroups[i];
      for (let j = 0; j < wishGroup.wishes.length; j++) {
        const wish = wishGroup.wishes[j];
        if (wish.id === wid) {
          return {
            id: wish.id,
            name: wish.name,
            gid: wishGroup.id,
            gname: wishGroup.name,
          };
        }
      }
    }
    return null;
  },

  getSelectedWishes: (state, commit, rootState) => (gid) => {
    try {
      return Object.keys(rootState.currentBasket.selectedWishes[gid]);
    } catch (e) {
      return [];
    }
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
    commit('setWishProduct', {
      gid,
      wid,
      pid,
      quantity,
    });
    resources.wishProduct.set({ gid, wid }, { pid, quantity });
  },

  updateWishProduct: ({ commit }, { gid, wid, pid, quantity }) => {
    commit('setWishProduct', {
      gid,
      wid,
      pid,
      quantity,
    });
    resources.wishProduct.save({ gid, wid }, { pid, quantity });
  },

  addWish({ commit }, { gid, name }) {
    return new Promise((resolve) => {
      resources.wishes.save({ gid }, { names: [name] }).then((response) => {
        const wid = response.body[0];
        commit('addWish', {
          gid,
          id: wid,
          name,
        });
        commit('selectWish', {
          gid,
          wid,
          selected: true,
        });
        resolve(wid);
      });
    });
  },

  removeWish: ({ commit, rootGetters }, { wid }) => {
    const wish = rootGetters.getWish(wid);
    const gid = wish.gid;
    resources.wish.delete({ gid, wid }, {}).then(() => {
      commit('removeWish', { wid });
    });
    commit('selectWish', { gid, wid, selected: false });
  },

  renameWish: ({ commit }, { gid, wid, name }) => {
    commit('renameWish', { wid, name });
    resources.wish.update({ gid, wid }, { name }).then(() => {
      // commit('renameWish', { wid, name });
    });
  },

  selectWish({ commit, rootGetters }, { wid, selected }) {
    return new Promise((resolve) => {
      const wish = rootGetters.getWish(wid);
      const gid = wish.gid;
      resources.wish.update({ gid, wid }, { selected }).then(() => {
        commit('selectWish', { gid, wid, selected });
        resolve();
      });
    });
  },
};

export default {
  getters,
  actions,
};
