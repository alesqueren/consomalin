import Vue from 'vue';
import resources from '../resources';

const globalGetters = {
  mergedBasketProducts: (state, rootGetters, rootState) => {
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
            let productNb = parseInt(product.quantity, 10);
            const priceByProduct = parseFloat(details[pid].price);
            let isMultiple = false;
            if (res[pid]) {
              productNb += parseInt(res[pid].productNb, 10);
              isMultiple = true;
            }
            const price = parseFloat((productNb * priceByProduct).toFixed(2));
            res[pid] = {
              productNb,
              priceByProduct,
              price,
              isMultiple,
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
      const mergedBasketProducts = rootGetters['basket/mergedBasketProducts'];
      commit('setBasketBeforePreparation', mergedBasketProducts);
      resources.prepareOrder.save(
        {
          basket: {
            totalPrice: parseFloat(rootGetters['transaction/basketAmount']),
            products: mergedBasketProducts,
          },
          slotId: rootState.singleton.selectedSlot.id,
        },
      ).then(({ body }) => {
        if (body !== 'OK') {
          const result = JSON.parse(body);
          const basket = result.basket;
          commit('setPreparedBasket', basket);
          const products = basket.products;
          const matchedWishes = rootGetters['selection/getMatchedWishes'];
          Object.keys(matchedWishes).map((wid) => {
            const wish = matchedWishes[wid];
            for (let i = 0; i < wish.length; i++) {
              const pid = wish[i].pid;
              if (products[pid]) {
                // console.log('pid in da basket');
              }
            }
            return true;
          });
          // dispatch('sectionWishes/update',
          //  () => commit('unselectGroup', { gid }),
          //  { root: true });
        }
        resolve();
        // return true;
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
