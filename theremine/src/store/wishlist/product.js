import resources from '../../resources';

const actions = {
  setProduct: ({ commit }, { gid, wid, pid, quantity }) => {
    commit('basket/setWishProduct', { gid, wid, pid, quantity }, { root: true });
    resources.wishProduct.save({ gid, wid }, { pid, quantity });
  },

  updateWishProduct: ({ commit }, { gid, wid, pid, quantity }) => {
    commit('basket/setWishProduct', { gid, wid, pid, quantity }, { root: true });
    resources.wishProduct.update({ gid, wid }, { pid, quantity });
  },
};

export default {
  namespaced: true,
  actions,
};
