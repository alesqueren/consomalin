import Vue from 'vue';
import resources from '../resources';

const globalGetters = {
  mergedBasketProductsBeforePreparation: (state, getters, rootState, rootGetters) => {
    const basket = rootState.selection.basket;
    const details = rootState.product.details;
    const res = {};
    res.products = {};
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
            if (res[pid]) {
              productNb += parseInt(res[pid].productNb, 10);
            }
            const price = parseFloat((productNb * priceByProduct).toFixed(2));
            res.products[pid] = {
              productNb,
              priceByProduct,
              price,
            };
          }
        }
      }
    }
    res.total = parseInt(rootGetters['transaction/basketAmount'], 10);
    return res;
  },
  mergedBasketProductsAfterPreparation: (state) => {
    const mergeBasketB = state.basketBeforePreparation;
    const diff = state.preparationDiff;
    const diffProducts = diff.products;
    const res = {};
    res.products = {};
    const fieldsToMerge = ['productNb', 'priceByProduct', 'price'];
    if (diffProducts) {
      for (const pid in mergeBasketB.products) {
        const product = mergeBasketB.products[pid];
        res.products[pid] = {};
        for (const fieldToMergeId in fieldsToMerge) {
          const fieldToMerge = fieldsToMerge[fieldToMergeId];
          const diffProduct = diffProducts[pid];
          if (diffProduct && diffProduct[fieldToMerge]) {
            res.products[pid][fieldToMerge] = diffProduct[fieldToMerge];
          } else {
            res.products[pid][fieldToMerge] = product[fieldToMerge];
          }
        }
      }
    }
    res.total = diff.totalPrice ? diff.totalPrice : mergeBasketB.total;
    return res;
  },
};

const actions = {
  prepareOrder({ dispatch, commit, rootGetters, rootState }) {
    return new Promise((resolve, reject) => {
      const mergedBasketProducts = rootGetters['basket/mergedBasketProductsBeforePreparation'];
      commit('setBasketBeforePreparation', mergedBasketProducts);
      resources.prepareOrder.save(
        {
          basket: {
            products: mergedBasketProducts.products,
            totalPrice: mergedBasketProducts.total,
          },
          slotId: rootState.singleton.selectedSlot.id,
        },
      ).then(({ body }) => {
        if (body === 'Something went wrong') {
          reject();
        } else if (body === 'OK') {
          commit('setIsPasketPrepared', true);
        } else if (body !== 'OK') {
          const result = JSON.parse(body);
          const basketDiff = result.basket;
          commit('setPreparationDiff', basketDiff);
          const products = basketDiff.products;
          const matchedWishes = rootGetters['selection/getMatchedWishes'];
          const newQties = {};
          Object.keys(matchedWishes).map((wid) => {
            const wish = matchedWishes[wid];
            for (let i = 0; i < wish.length; i++) {
              const pid = wish[i].pid;
              const product = products[pid];
              if (product && product.productNb) {
                newQties[pid] = {
                  maxQty: product.productNb,
                  remindedQty: product.productNb,
                };
              }
            }
            return true;
          });
          Object.keys(matchedWishes).map((wid) => {
            const wish = matchedWishes[wid];
            // process new quantity
            for (let i = 0; i < wish.length; i++) {
              const pid = wish[i].pid;
              const oldNb = wish[i].quantity;
              const product = newQties[pid];
              if (product) {
                const remindedQty = product.remindedQty;
                const diff = remindedQty - oldNb;
                if (diff < 0) {
                  const newNb = oldNb + diff;
                  if (newNb <= 0) {
                    dispatch('selection/removeProduct', { wid, pid }, { root: true });
                  } else {
                    dispatch('selection/updateProduct', { wid, pid, quantity: newNb }, { root: true });
                    newQties[pid].remindedQty -= newNb;
                  }
                } else {
                  newQties[pid].remindedQty = diff;
                }
              }
            }
            return true;
          });
        }
        commit('setIsPasketPrepared', true);
        const mergedBasket2 = rootGetters['basket/mergedBasketProductsAfterPreparation'];
        commit('setBasketAfterPreparation', mergedBasket2);
        resolve();
      });
    });
  },
  order({ commit, rootGetters, rootState }) {
    return new Promise((resolve, reject) => {
      const mergedBasketProducts = rootGetters['basket/mergedBasketProductsAfterPreparation'];
      commit('setBasketBeforePreparation', mergedBasketProducts);
      resources.order.save(
        {
          basket: {
            products: mergedBasketProducts.products,
            totalPrice: mergedBasketProducts.total,
          },
          slotId: rootState.singleton.selectedSlot.id,
        },
      ).then(({ body }) => {
        if (body === 'Something went wrong') {
          reject();
        } else if (body === 'OK') {
          commit('setIsPasketPrepared', true);
        } else if (body !== 'OK') {
          const result = JSON.parse(body);
        }
        resolve();
      });
    });
  },
  setIsPasketPrepared({ commit }) {
    commit('setIsPasketPrepared', false);
  },
};

const mutations = {
  setBasketBeforePreparation: (state, basketBeforePreparation) => {
    Vue.set(state, 'basketBeforePreparation', basketBeforePreparation);
    Vue.set(state.basketBeforePreparation, 'tmp');
    delete state.basketBeforePreparation.tmp;
  },
  setBasketAfterPreparation: (state, basketAfterPreparation) => {
    Vue.set(state, 'basketAfterPreparation', basketAfterPreparation);
    Vue.set(state.basketAfterPreparation, 'tmp');
    delete state.basketAfterPreparation.tmp;
  },
  setPreparationDiff: (state, preparationDiff) => {
    Vue.set(state, 'preparationDiff', preparationDiff);
    Vue.set(state.preparationDiff, 'tmp');
    delete state.preparationDiff.tmp;
  },
  setIsPasketPrepared: (state, isBasketPrepared) => {
    Vue.set(state, 'isBasketPrepared', isBasketPrepared);
  },
};


export default {
  namespaced: true,
  state: {
    basketBeforePreparation: {},
    preparationDiff: {},
    basketAfterPreparation: {},
    isBasketPrepared: false,
  },
  getters: globalGetters,
  actions,
  mutations,
};
