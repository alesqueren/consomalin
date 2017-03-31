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

  getWishlist: (state, commit, rootState) => {
    const wishlist = [];
    if (rootState.wishGroups) {
      const selectedWishes = rootState.currentBasket.selectedWishes;
      for (let i = 0; i < rootState.wishGroups.length; i++) {
        const wishgroup = rootState.wishGroups[i];
        const newWishGroup = {
          id: wishgroup.id,
          name: wishgroup.name,
          wishes: [],
          selected: false,
        };
        const wishGroupLength = wishgroup.wishes ? wishgroup.wishes.length : 0;
        for (let j = 0; j < wishGroupLength; j++) {
          const wish = rootState.wishGroups[i].wishes[j];
          const newWish = {
            id: wish.id,
            name: wish.name,
            gid: wishgroup.id,
            selected: false,
          };
          if (selectedWishes) {
            const wishGroupSelect = selectedWishes[newWish.gid];
            if (wishGroupSelect && wishGroupSelect[newWish.id]) {
              newWish.selected = true;
              newWishGroup.selected = true;
            }
          }
          newWishGroup.wishes.push(newWish);
        }
        wishlist.push(newWishGroup);
      }
    }
    return wishlist;
  },
};

export default {
  getters,
};
