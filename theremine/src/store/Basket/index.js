import resources from '../../resources';

function getFirstUnmatchedSelectedWish(basket) {
  for (let i = 0; i < basket.length; i++) {
    const wish = basket[i];
    if (!wish.product.id) {
      wish.current = true;
      return { groupid: wish.groupId, wishid: wish.id };
    }
  }
  return null;
}

const getters = {
  getBasket: (state, commit, rootState) => {
    const basket = [];
    if (rootState.wishGroups && rootState.currentBasket.selectedWishes) {
      const selectedWishes = rootState.currentBasket.selectedWishes;
      for (let i = 0; i < rootState.wishGroups.length; i++) {
        const wishgroup = rootState.wishGroups[i];
        const wishGroupLength = wishgroup.wishes ? wishgroup.wishes.length : 0;
        for (let j = 0; j < wishGroupLength; j++) {
          const wish = rootState.wishGroups[i].wishes[j];
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
              selected: true,
            };
            basket.push(newWish);
          }
        }
      }
    }
    return basket;
  },
};
const actions = {
  updateWishGroupsAndCurrentBasket: ({ dispatch, commit, rootState }) => {
    if (!rootState.wishGroups) {
      resources.wishlist.get(
        {},
        {},
      ).then((response) => {
        const data = JSON.parse(response.data);
        const wishGroups = data.wishGroups;
        const currentBasket = data.currentBasket;
        if (!currentBasket.selectedWishes) {
          currentBasket.selectedWishes = {};
        }
        if (!rootState.currentBasket.currentWish) {
          const currentWish = getFirstUnmatchedSelectedWish(this.basket);
          console.log('1');
          if (currentWish) {
            console.log('2');
            dispatch('setCurrentWish', { currentWish });
          }
        }
        if (rootState.currentBasket.currentWish) {
          dispatch('searchProductsForWish', { wish: rootState.currentBasket.currentWish });
          this.searchProducts(this.basket.currentWish);
        }
        commit('setWishGroupsAndCurrentBasket', { wishGroups, currentBasket });
      }, () => {
        // console.log('error');
      });
    }
  },
  searchProductsForWish: ({ commit }, { wish }) => {
    resources.currentWish.save({}, { pid: wish.product.id }).then((response) => {
      // commit('setCurrentWish', { id: response.body, name, wishes: [] });
    }, () => {
      // console.log('error');
    });
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
