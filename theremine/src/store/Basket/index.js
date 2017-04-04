import resources from '../../resources';

const getters = {
  getBasketWish: (state, commit, rootState) => (wid) => {
    const basket = rootState.getBasket;
    for (let i = 0; i < basket.length; i++) {
      const wishGroup = basket[i];
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
            const newWish = {
              id: wish.id,
              name: wish.name,
              gid: wishgroup.id,
              gname: wishgroup.name,
              product: {
                id: selectedWish.pid,
                quantity: selectedWish.quantity,
                infos: productInfos,
              },
            };
            if (currentBasket.currentWishId === wish.id) {
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
  searchProductsWithName: ({ commit, rootState }, { name }) => {
    if (!rootState.searchs.name) {
      const uri = 'search?s=' + name;
      resources.products.get({ uri }, {}).then((response) => {
        const products = JSON.parse(response.body);
        commit('addSearchs', { name, products });
      });
    }
  },

  setSlots({ commit, rootState }) {
    return new Promise((resolve, reject) => {
      resources.schedule.get().then((response) => {
        const slots = JSON.parse(response.body);
        console.log('slots action');
        console.log(slots);
        commit('setSlots', { slots });
        resolve();
      });
    });
  },

  detailProductsWithId: ({ commit, rootState }, { ids }) => {
    const uri = 'details?pids=' + JSON.stringify(ids);
    resources.products.get({ uri }, {}).then((response) => {
      const products = JSON.parse(response.body);
      Object.keys(products).map((pid) => {
        const infos = products[pid];
        commit('addProductInfos', { pid, infos });
        return null;
      });
    });
  },

  updateProductInfos: ({ commit, rootState }, { pid, infos }) => {
    if (!rootState.productInfos[pid]) {
      commit('addProductInfos', { pid, infos });
    }
  },

  setCurrentWish({ commit }, { wid }) {
    return new Promise((resolve) => {
      resources.currentWish.save({}, { wid }).then(() => {
        commit('setCurrentWish', { wid });
        resolve();
      });
    });
  },
  order({ commit }) {
    return new Promise((resolve) => {
      resources.order.save().then(() => {
        resolve();
      });
    });
  },
  slot({ commit }, { slotId }) {
    return new Promise((resolve) => {
      resources.slot.save({}, slotId).then(() => {
        commit('setCurrentWish', { slotId });
        resolve();
      });
    });
  },
};

export default {
  getters,
  actions,
};
