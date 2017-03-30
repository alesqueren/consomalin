import resources from '../../resources';

const getters = {
  isSelectedWishGroup: (state, commit, rootState) => (gid) => {
    const selectedWishes = rootState.currentBasket.selectedWishes;
    return Boolean(selectedWishes && selectedWishes[gid]);
  },
};

const actions = {
  addWishGroup: ({ commit }, name) => {
    resources.wishgroup.save({}, { name }).then((response) => {
      commit('addWishGroup', { id: response.body, name, wishes: [] });
    });
  },
  selectWishGroup: ({ commit }, { gid, selected }) => {
    const commitName = selected ? 'selectGroup' : 'unselectGroup';
    commit(commitName, { gid });
    resources.wishgroup.update({ gid }, { selected });
  },
  removeWishGroup: ({ commit }, gid) => {
    resources.wishgroup.delete({ gid }, {}).then(() => {
      commit('unselectGroup', { gid });
      commit('removeWishGroup', { gid });
    });
  },
};

export default {
  getters,
  actions,
};
