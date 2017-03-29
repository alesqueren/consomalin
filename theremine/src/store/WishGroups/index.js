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
    console.log('action selectWishGroup ' + groupId + ' ' + selected);
    resources.wishgroup.update({ groupid: groupId }, { selected });
    // resources.wishgroup.delete({ groupid: groupId }, {}).then(() => {
    //   commit('selectWishGroup', { groupId });
    // });
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
