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
        const body = JSON.parse(response.body);
        commit('setSlots', { slots: body.slots });
        resolve();
      });
    });
  },

  selectSlot({ commit, rootState }, { slotId }) {
    return new Promise((resolve, reject) => {
      let date = '';
      let time = '';
      let frenchTime = '';
      const daySlots = rootState.slots;
      for (let i = 0; i < daySlots.length; i++) {
        const day = daySlots[i];
        for (let j = 0; j < day.slots.length; j++) {
          const slotHours = day.slots[j];
          if (slotHours) {
            for (let k = 0; k < slotHours.length; k++) {
              const slot = slotHours[k];
              if (slot.id === slotId) {
                date = slot.day;
                time = slot.time;
                const hours = parseInt(time.split(':')[0], 10);
                const minutes = time.split(':')[1];
                frenchTime = day.name + ' ' + hours + 'h' + minutes;
              }
            }
          }
        }
      }
      const dateTime = date + ' ' + time;
      resources.slot.save({}, { id: slotId, dateTime }).then((response) => {
        commit('selectSlot', { slotId });
        resolve(frenchTime);
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
