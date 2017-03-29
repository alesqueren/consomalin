import resources from '../../resources';

const getters = {
  isSelectedWishGroup: (state, commit, rootState) => (groupId) => {
    const selectedWishes = rootState.currentBasket.selectedWishes;
    return (selectedWishes && selectedWishes[groupId]);
  },
};

const actions = {
  addWishGroup: ({ commit }, name) => {
    resources.wishgroup.save({}, { name }).then((response) => {
      commit('addWishGroup', { id: response.body, name, wishes: [] });
    });
  },
  selectWishGroup: ({ commit }, { groupId, selected }) => {
    resources.wishgroup.update({ groupid: groupId }, { selected });
    if (selected) {
      commit('selectGroup', { groupId });
    } else {
      commit('unselectGroup', { groupId });
    }
  },
  removeWishGroup: ({ commit }, groupId) => {
    resources.wishgroup.delete({ groupid: groupId }, {}).then(() => {
      commit('removeWishGroup', { groupId });
    });
  },
};

export default {
  getters,
  actions,
};
