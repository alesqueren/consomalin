import Vue from 'vue';
import resources from '../resources';

const globalGetters = {
  mergedBasketContent: (state, rootGetters, rootState) => {
    const basket = rootState.selection.basket;
    const details = rootState.product.details;
    const res = {};
    for (const gid in basket) {
      const group = basket[gid];
      for (const wid in group) {
        const wish = group[wid];
        for (const index in wish) {
          const product = wish[index];
          const pid = product.pid;
          if (details[pid]) {
            let productNb = product.quantity;
            const priceByProduct = details[pid].price;
            if (res[pid]) {
              productNb += res[pid].productNb;
            }
            res[pid] = {
              productNb,
              priceByProduct,
              price: (productNb * priceByProduct).toFixed(2),
            };
          }
        }
      }
    }
    return res;
  },
};

const actions = {
  prepareOrder({ commit, rootGetters, rootState }) {
    return new Promise((resolve) => {
      const mergedBasketContent = rootGetters['basket/mergedBasketContent'];
      console.log(mergedBasketContent);
      resources.prepareOrder.save(
        {
          basket: {
            totalPrice: parseFloat(rootGetters['transaction/basketAmount']),
            products: mergedBasketContent,
          },
          slotId: rootState.singleton.selectedSlot.id,
        },
      ).then(({ body }) => {
        const parsedBody = JSON.parse(body);
        commit('set', {
          setPreparedBasket: parsedBody,
        });
        resolve();
      });
    });
  },
};

const mutations = {
  setPreparedBasket: (state, preparedBasket) => {
    Vue.set(state, 'preparedBasket', preparedBasket);
  },
};


export default {
  namespaced: true,
  state: {
    preparedBasket: {},
  },
  getters: globalGetters,
  actions,
  mutations,
};
