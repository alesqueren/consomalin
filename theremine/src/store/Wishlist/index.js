const getters = {
  getWishlist: (state, commit, rootState) => {
    const wishlist = [];
    // console.log(rootState.wishGroups);
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
            groupId: wishgroup.id,
            selected: false,
          };
          if (selectedWishes) {
            const wishGroupSelect = selectedWishes[newWish.groupId];
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
