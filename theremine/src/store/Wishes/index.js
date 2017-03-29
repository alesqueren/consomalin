import resources from '../../resources';

const getters = {
  getWish: (state, commit, rootState) => (wishId) => {
    let wishFound = null;
    for (let i = 0; i < rootState.wishGroups.length; i++) {
      const wishGroup = rootState.wishGroups[i];
      for (let j = 0; j < wishGroup.wishes.length; j++) {
        const wish = wishGroup.wishes[j];
        if (wish.id === wishId) {
          wishFound = {
            id: wish.id,
            name: wish.name,
            groupId: wishGroup.id,
            groupName: wishGroup.name,
          };
        }
      }
    }
    return wishFound;
  },
  getCurrentWish(state, commit, rootState) {
    let wishFound = {};
    if (rootState.currentBasket && rootState.currentBasket.currentWish) {
      const currentWish = rootState.currentBasket.currentWish;
      for (let i = 0; i < rootState.wishGroups.length; i++) {
        const wishGroup = rootState.wishGroups[i];
        for (let j = 0; j < wishGroup.wishes.length; j++) {
          const wish = wishGroup.wishes[j];
          if (wish.id === currentWish.wishId) {
            wishFound = {
              id: wish.id,
              name: wish.name,
              groupId: wishGroup.id,
              groupName: wishGroup.name,
            };
          }
        }
      }
    }
    return wishFound;
  },
  isSelectedWish: (state, commit, rootState) => ({ groupId, wishId }) => {
    const sw = rootState.currentBasket.selectedWishes;
    if (!sw) {
      sw.selectedWishes = {};
    }
    const hasGrpIdP = Object.prototype.hasOwnProperty.call(sw, groupId);
    if (hasGrpIdP) {
      return Object.prototype.hasOwnProperty.call(sw[groupId], wishId);
    }
    return false;
  },
};
const actions = {
  setProduct: ({ commit }, { groupId, wishId, pid, quantity }) => {
    resources.wishProduct.save({ groupId, wishId }, { pid, quantity }).then((response) => {
      commit('setProduct', {
        groupId,
        wishId,
        pid,
        quantity,
      });
    });
  },
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
    });
  },
  removeWish: ({ commit }, { groupId, wishId }) => {
    resources.wish.delete(
      {
        groupid: groupId,
        wishid: wishId,
      }, {}).then(() => {
        commit('removeWish', { wishId });
      },
    );
    commit('selectWish', { groupId, wishId, selected: false });
  },
  renameWish: ({ commit }, { groupId, wishId, name }) => {
    commit('renameWish', { wishId, name });
    resources.wish.update(
      {
        groupid: groupId,
        wishid: wishId,
      }, { name }).then(() => {
        // commit('renameWish', { wishId, name });
      },
    );
  },
  selectWish: ({ commit }, { groupId, wishId, selected }) => {
    resources.wish.update(
      {
        groupid: groupId,
        wishid: wishId,
      },
      { selected }).then(() => {
        commit('selectWish', { groupId, wishId, selected });
      },
    );
  },
};

export default {
  getters,
  actions,
};
