// import resources from '../../resources';

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
            const productInfos = rootState.productInfos[selectedWish.pid];
            console.log('productInfos : ' + selectedWish.pid);
            console.log(productInfos);
            const newWish = {
              id: wish.id,
              name: wish.name,
              groupId: wishgroup.id,
              product: {
                id: selectedWish.pid,
                quantity: selectedWish.quantity,
                infos: productInfos,
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
  searchProductsForName: ({ commit, rootState }, { name }) => {
    if (!rootState.searchs.name) {
      const uri = 'search?s=' + name;
      resources.kiva.get({ uri }, {}).then((response) => {
        const products = JSON.parse(response.body);
        commit('addSearchs', { name, products });
      });
    }
  },
  updateProductInfos: ({ commit, rootState }, { pid, name, imageUrl, price }) => {
    const infos = {
      name,
      imageUrl,
      price,
    };
    if (!rootState.productInfos[pid]) {
      commit('addProductInfos', { pid, infos });
    }
  },
  setCurrentWish: ({ commit }, { groupid, wishid }) => {
    // console.log('store basket actions setCurrentWish');
    resources.currentWish.save({}, { groupid, wishid }).then((response) => {
      commit('setCurrentWish', { groupid, wishid });
    });
  },
};

export default {
  getters,
  actions,
};
