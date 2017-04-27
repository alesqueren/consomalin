import resources from '../resources';

const globalGetters = {
  basketAmount: (state, getters, rootState, rootGetters) => {
    const basket = rootGetters['selection/getOrderedSelectedWishes'];
    return basket.reduce((prev, wid) => {
      let priceProduct = 0;
      const products = rootGetters['selection/getMatchedWishes'][wid];
      for (const i in products) {
        const product = products[i];
        const pid = product.pid;
        const quantity = product.quantity;
        const details = rootState.product.details[pid];
        if (details) {
          const price = details.price;
          priceProduct = price ? priceProduct + (price * quantity) : priceProduct + 0;
        }
      }
      return prev + priceProduct;
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
