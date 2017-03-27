import resources from '../../resources';

const getters = {
  getWish: (state, commit, rootState) => (wishId) => {
    let wishFound = null;
    for (let i = 0; i < rootState.wishGroups.length; i++) {
      const wishGroup = rootState.wishGroups[i];
      for (let j = 0; j < wishGroup.wishes.length; j++) {
        const wish = wishGroup.wishes[j];
        if (wish.id === wishId) {
          wishFound = wish;
        }
      }
    }
    return wishFound;
  },
  isSelectedWish: (state, commit, rootState) => ({ groupId, wishId }) => {
    const sw = rootState.currentBasket.selectedWishes;
    // console.log('isSelectedWish return');
    const hasGrpIdP = Object.prototype.hasOwnProperty.call(sw, groupId);
    // console.log((hasGrpIdP && Object.prototype.hasOwnProperty.call(sw[groupId], wishId)));
    if (hasGrpIdP) {
      return Object.prototype.hasOwnProperty.call(sw[groupId], wishId);
    }
    return false;
  },
};
const actions = {
  addWish: ({ commit }, { group, name }) => {
    const groupid = group.id;
    resources.wishes.save({ groupid }, { names: [name] }).then((response) => {
      commit('addWish', {
        groupId: group.id,
        id: response.body,
        name,
      });
      commit('selectWish', {
        groupId: group.id,
        wishId: response.body,
        selected: true,
      });
    }, () => {
      // console.log('error');
    });
  },
  removeWish: ({ commit }, { groupId, wishId }) => {
    resources.wish.delete({ groupid: groupId, wishid: wishId }, {}).then(() => {
      commit('removeWish', { wishId });
    }, () => {
      // console.log('error');
    });
    commit('selectWish', { groupId, wishId, selected: false });
  },
  selectWish: ({ commit }, { groupId, wishId, selected }) => {
    // console.log('store:action:selectWish: wishId');
    resources.wish.update(
      {
        groupid: groupId,
        wishid: wishId,
      },
      { selected }).then(() => {
        commit('selectWish', { groupId, wishId, selected });
      }, () => {
      // console.log('error');
      },
    );
  },
};

export default {
  getters,
  actions,
};
