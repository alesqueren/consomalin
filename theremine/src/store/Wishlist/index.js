const getters = {

  isEditing: (state, commit, rootState) => id =>
    Boolean(rootState.inlineEdition === id),

  getSelectedWishGroups: (state, commit, rootState) => {
    try {
      return Object.keys(rootState.currentBasket.selectedWishes);
    } catch (e) {
      return [];
    }
  },

  getGroups: (state, commit, rootState) => {
    const res = [];
    if (rootState.wishGroups) {
      for (let i = 0; i < rootState.wishGroups.length; i++) {
        res.push(rootState.wishGroups[i].id);
      }
    }
    return res;
  },

  getActiveWishGroup: (state, commit, rootState) => {
    const gid = rootState.activeWishGroup;
    if (gid && rootState.wishGroups) {
      for (let i = 0; i < rootState.wishGroups.length; i++) {
        const currGroup = rootState.wishGroups[i];
        if (currGroup.id === gid) {
          return currGroup;
        }
      }
    }
    return null;
  },
};

export default {
  getters,
};
