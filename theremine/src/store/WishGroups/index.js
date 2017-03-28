import resources from '../../resources';

const getters = {
  isSelectedWishGroup: (state, commit, rootState) => ({ groupId }) => {
    const selectedWishes = rootState.currentBasket.selectedWishes;
    return (selectedWishes && selectedWishes[groupId]);
  },
};
const actions = {
  addWishGroup: ({ commit }, name) => {
    resources.wishgroup.save({}, { name }).then((response) => {
      commit('addWishGroup', { id: response.body, name, wishes: [] });
    }, () => {
      // console.log('error');
    });
  },
  removeWishGroup: ({ commit }, groupId) => {
    resources.wishgroup.delete({ groupid: groupId }, {}).then(() => {
      commit('removeWishGroup', { groupId });
    }, () => {
      // console.log('error');
    });
    commit('selectWishGroup', { groupId, selected: false });
  },
};

export default {
  getters,
  actions,
};
