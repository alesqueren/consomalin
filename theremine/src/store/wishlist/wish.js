import resources from '../../resources';

const actions = {
  add({ commit }, { gid, name }) {
    return new Promise((resolve) => {
      resources.wish.bulk({ gid }, { names: [name] }).then((response) => {
        const wid = response.body[0];
        commit('wishlist/group/addWish', { gid, id: wid, name }, { root: true });
        commit('basket/selectWish', { gid, wid, selected: true }, { root: true });
        resolve(wid);
      });
    });
  },

  rename: ({ commit }, { gid, wid, name }) => {
    commit('wishlist/group/renameWish', { wid, name }, { root: true });
    resources.wish.update({ gid, wid }, { name });
  },

  remove: ({ commit, rootGetters }, { wid }) => {
    const gid = rootGetters['wishlist/group/getByWish'](wid).gid;
    resources.wish.delete({ gid, wid }).then(() => { });
    commit('wishlist/group/removeWish', { wid }, { root: true });
    commit('basket/selectWish', { gid, wid, selected: false }, { root: true });
  },

};

export default {
  namespaced: true,
  actions,
};
