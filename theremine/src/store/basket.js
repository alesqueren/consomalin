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
            const price = parseFloat(parseInt(productNb, 10) * parseFloat(priceByProduct));
            res[pid] = {
              productNb,
              priceByProduct,
              price,
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
      commit('setBasketBeforePreparation', mergedBasketContent);
      resources.prepareOrder.save(
        {
          basket: {
            totalPrice: parseFloat(rootGetters['transaction/basketAmount']),
            products: mergedBasketContent,
          },
          slotId: rootState.singleton.selectedSlot.id,
        },
      ).then(({ body }) => {
        if (body !== 'OK') {
          const result = JSON.parse(body);
          const message = result.message;
          const basket = result.basket;
          commit('setPreparedBasket', basket);
        }
        resolve();
        resolve();
      });
    });
  },
};

const mutations = {
  setBasketBeforePreparation: (state, basketBeforePreparation) => {
    Vue.set(state, 'basketBeforePreparation', basketBeforePreparation);
    Vue.set(state.basketBeforePreparation, 'tmp');
    delete state.basketBeforePreparation.tmp;
  },
  setPreparedBasket: (state, preparedBasket) => {
    Vue.set(state, 'preparedBasket', preparedBasket);
    Vue.set(state.preparedBasket, 'tmp');
    delete state.preparedBasket.tmp;
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
