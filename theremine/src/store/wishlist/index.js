import group from './group';
import wish from './wish';
import product from './product';

const getters = {
  getGroups: (state) => {
    const res = [];
    if (state.group.wishGroups) {
      for (let i = 0; i < state.group.wishGroups.length; i++) {
        res.push(state.group.wishGroups[i].id);
      }
    }
    return res;
  },
};

export default {
  namespaced: true,
  getters,
  modules: {
    group,
    wish,
    product,
  },
};
