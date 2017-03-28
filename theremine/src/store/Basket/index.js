import resources from '../../resources';

const getters = {
  getBasket: (state, commit, rootState) => {
    const basket = [];
    const currentBasket = rootState.currentBasket;
    const wishGroups = rootState.wishGroups;
    if (wishGroups && currentBasket.selectedWishes) {
      const selectedWishes = currentBasket.selectedWishes;
      for (let i = 0; i < wishGroups.length; i++) {
        const wishgroup = wishGroups[i];
        const wishGroupLength = wishgroup.wishes ? wishgroup.wishes.length : 0;
        for (let j = 0; j < wishGroupLength; j++) {
          const wish = wishGroups[i].wishes[j];
          const wishGroupSelect = selectedWishes[wishgroup.id];
          if (wishGroupSelect && selectedWishes[wishgroup.id][wish.id]) {
            const selectedWish = selectedWishes[wishgroup.id][wish.id];
            const newWish = {
              id: wish.id,
              name: wish.name,
              groupId: wishgroup.id,
              matchingProducts: [],
              product: {
                id: selectedWish.pid,
                quantity: selectedWish.quantity,
              },
            };
            const sameGroup = currentBasket.currentWish.groupid === wishgroup.id;
            // console.log('currentBasket.currentWish.groupid');
            // console.log(currentBasket.currentWish.groupid);
            const sameWish = currentBasket.currentWish.wishid === wish.id;
            if (sameGroup && sameWish) {
              newWish.current = true;
            }
            basket.push(newWish);
          }
        }
      }
    }
    return basket;
  },
};
const actions = {
  searchProductsForWish: ({ commit }, { wish }) => {
    // resources.currentWish.save({}, { pid: wish.product.id }).then((response) => {
    // console.log('response');
    // console.log(response);
    // commit('setMatchingProducts', { wish, response });
    // }, () => {
    //  console.log('error');
    // });
  },
  setCurrentWish: ({ commit }, { groupid, wishid }) => {
    // resources.currentWish.save({}, { groupid, wishid }).then((response) => {
    commit('setCurrentWish', { groupid, wishid });
    // }, () => {
      // console.log('error');
    // });
  },
};

export default {
  getters,
  actions,
};
