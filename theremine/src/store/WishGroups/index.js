import resources from '../../resources';

const getters = {
  isSelectedWishGroup: (state, commit, rootState) => (groupId) => {
    const selectedWishes = rootState.currentBasket.selectedWishes;
    return Boolean(selectedWishes && selectedWishes[groupId]);
  },
};

const actions = {
  addWishGroup: ({ commit }, name) => {
    resources.wishgroup.save({}, { name }).then((response) => {
      commit('addWishGroup', { id: response.body, name, wishes: [] });
    });
  },
  selectWishGroup: ({ commit }, { groupId, selected }) => {
    const commitName = selected ? 'selectGroup' : 'unselectGroup';
    commit(commitName, { groupId });
    resources.wishgroup.update({ groupid: groupId }, { selected });
  },
  removeWishGroup: ({ commit }, groupId) => {
    resources.wishgroup.delete({ groupid: groupId }, {}).then(() => {
      commit('unselectGroup', { groupId });
      commit('removeWishGroup', { groupId });
    });
  },
};

export default {
  getters,
  actions,
};
