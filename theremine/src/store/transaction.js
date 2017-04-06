import resources from '../resources';

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
  actions,
};
