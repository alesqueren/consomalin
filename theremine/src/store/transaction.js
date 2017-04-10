import resources from '../resources';

const globalGetters = {
  basketAmount: (state, getters, rootState, rootGetters) => {
    const basket = rootGetters['selection/getOrdreredSelectedWishes'];
    return basket.reduce((prev, wid) => {
      const product = rootGetters['selection/getMatchedWishes'][wid];
      if (product) {
        const pid = product.pid;
        const quantity = product.quantity;
        const details = rootState.product.details[pid];
        if (details) {
          const price = details.price;
          const priceProduct = price ? price * quantity : 0;
          return prev + priceProduct;
        }
        return prev;
      }
      return prev;
    }, 0).toFixed(2);
  },
};

const actions = {

  order() {
    return new Promise((resolve) => {
      resources.order.save().then(() => {
        resolve();
      });
    });
  },
};

export default {
  namespaced: true,
  getters: globalGetters,
  actions,
};
