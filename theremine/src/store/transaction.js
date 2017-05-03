import resources from '../resources';

function add(rootState, rootGetters) {
  return (acc, wid) => {
    let priceProduct = 0;
    const products = rootGetters['selection/getMatchedWishes'][wid];
    for (const i in products) {
      const pd = products[i];
      const details = rootState.product.details[pd.pid];
      if (details) {
        const price = details.price;
        priceProduct = price ? priceProduct + (price * pd.quantity) : priceProduct + 0;
      }
    }
    return acc + priceProduct;
  };
}

const globalGetters = {
  basketAmount: (state, getters, rootState, rootGetters) => {
    const basket = rootGetters['selection/getOrderedSelectedWishes'];
    return basket.reduce(add(rootState, rootGetters), 0).toFixed(2);
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
