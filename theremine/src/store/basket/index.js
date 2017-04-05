import Vue from 'vue';
import resources from '../../resources';

// TODO: rewrite with only state
const globalGetters = {
  isSelectedWishGroup: state => gid =>
    Boolean(state.selectedWishes && state.selectedWishes[gid]),

  // TODO: rm ?
  getBasket: (state, commit, rootState) => {
    const basket = [];
    const wishGroups = rootState.wishlist.group.wishGroups;
    if (wishGroups && state.selectedWishes) {
      const selectedWishes = state.selectedWishes;
      for (let i = 0; i < wishGroups.length; i++) {
        const wishgroup = wishGroups[i];
        const wishGroupLength = wishgroup.wishes ? wishgroup.wishes.length : 0;
        for (let j = 0; j < wishGroupLength; j++) {
          const wish = wishGroups[i].wishes[j];
          const wishGroupSelect = selectedWishes[wishgroup.id];
          if (wishGroupSelect && selectedWishes[wishgroup.id][wish.id]) {
            const selectedWish = selectedWishes[wishgroup.id][wish.id];
            const productInfos = rootState.productInfos[selectedWish.pid];
            const newWish = {
              id: wish.id,
              name: wish.name,
              gid: wishgroup.id,
              gname: wishgroup.name,
              product: {
                id: selectedWish.pid,
                quantity: selectedWish.quantity,
                infos: productInfos,
              },
            };
            if (state.currentWishId === wish.id) {
              newWish.current = true;
            }
            basket.push(newWish);
          }
        }
      }
    }
    return basket;
  },

  getSelectedWishGroups: (state) => {
    try {
      return Object.keys(state.selectedWishes);
    } catch (e) {
      return [];
    }
  },

  getSelectedWishes: state => (gid) => {
    try {
      return Object.keys(state.selectedWishes[gid]);
    } catch (e) {
      return [];
    }
  },

  isSelectedWish: ({ selectedWishes }) => ({ gid, wid }) => {
    if (!selectedWishes) {
      selectedWishes = {};
    }
    const hasGrpIdP = Object.prototype.hasOwnProperty.call(selectedWishes, gid);
    if (hasGrpIdP) {
      return Object.prototype.hasOwnProperty.call(selectedWishes[gid], wid);
    }
    return false;
  },

};

const actions = {
  selectWish({ commit, rootGetters }, { wid, selected }) {
    return new Promise((resolve) => {
      const gid = rootGetters['wishlist/group/getByWish'](wid).gid;
      resources.wish.update({ gid, wid }, { selected }).then(() => {
        commit('selectWish', { gid, wid, selected });
        resolve();
      });
    });
  },

  selectWishGroup: ({ commit }, { gid, selected }) => {
    const commitName = selected ? 'selectGroup' : 'basket/unselectGroup';
    commit(commitName, { gid }, { root: true });
    resources.wishgroup.update({ gid }, { selected });
  },

  setSlots({ commit }) {
    return new Promise((resolve) => {
      resources.schedule.get().then((response) => {
        const slots = JSON.parse(response.body);
        commit('setSlots', { slots });
        resolve();
      });
    });
  },

  detailProductsWithId: ({ commit, rootState }, { ids }) => {
    const uri = 'details?pids=' + JSON.stringify(ids);
    resources.products.get({ uri }, {}).then((response) => {
      const products = JSON.parse(response.body);
      Object.keys(products).map((pid) => {
        const infos = products[pid];
        commit('addProductInfos', { pid, infos }, { root: true });
        return null;
      });
    });
  },

  setCurrentWish({ commit }, { wid }) {
    return new Promise((resolve) => {
      resources.currentWish.save({}, { wid }).then(() => {
        commit('setCurrentWish', { wid });
        resolve();
      });
    });
  },

  order() {
    return new Promise((resolve) => {
      resources.order.save().then(() => {
        resolve();
      });
    });
  },

  slot({ commit }, { slotId }) {
    return new Promise((resolve) => {
      resources.slot.save({}, slotId).then(() => {
        commit('setCurrentWish', { slotId });
        resolve();
      });
    });
  },
};

const mutations = {
  removeCurrentWish: (state) => {
    Vue.set(state, 'currentWishId', null);
  },

  setCurrentWish: (state, { wid }) => {
    Vue.set(state, 'currentWishId', wid);
  },

  setWishProduct: (state, { gid, wid, pid, quantity }) => {
    const entity = state.selectedWishes[gid][wid];
    Vue.set(entity, 'pid', pid);
    Vue.set(entity, 'quantity', quantity);
  },

  selectWish: ({ selectedWishes }, { gid, wid, selected }) => {
    // si on deselectionne un wish
    if (!selected) {
      if (selectedWishes[gid]) {
        Vue.set(selectedWishes[gid], wid);
        delete selectedWishes[gid][wid];

        Vue.set(selectedWishes[gid], 'tmp');
        delete selectedWishes[gid].tmp;

        // si on a supprimé le dernier wish, on supprime le groupe de l'objet
        if (!Object.keys(selectedWishes[gid]).length) {
          Vue.set(selectedWishes, 'tmp');
          delete selectedWishes.tmp;

          Vue.set(selectedWishes, gid);
          delete selectedWishes[gid];
        }
      }
    } else {
      // si le groupe n'existe pas, on le crée
      if (!selectedWishes[gid]) {
        Vue.set(selectedWishes, gid, {});
      }
      // dans tous les cas on rajoute le wish a son groupe
      Vue.set(selectedWishes[gid], wid);
    }
  },

  unselectGroup: (state, { gid }) => {
    if (state.selectedWishes[gid]) {
      Vue.set(state.selectedWishes, gid, null);
      delete state.selectedWishes[gid];
    }
  },

  selectSlot: (state, slotId) => {
    state.slot = slotId;
  },

  setSlots: (state, { slots }) => {
    const days = ['Dimanche', 'Lundi', 'Mardi', 'Mercredi', 'Jeudi', 'Vendredi', 'Samedi'];
    const newSlots = [];
    let daySlots = [];
    for (let i = 0; i < slots.length; i++) {
      const slot = slots[i];
      slot.selected = false;

      const d = new Date(slot.day);
      const h = new Date(slot.day + ' ' + slot.time);
      let dayName = '';
      // au dernier slot du jour, on les ajoutes tous
      if ((dayName !== days[d.getDay()] || i + 1 === slots.length) && !i === 0) {
        newSlots.push({ name: dayName, slots: daySlots });
        daySlots = [];
      }
      dayName = days[d.getDay()];
      if (!Object.keys(daySlots[h.getHours()]).length) {
        daySlots[h.getHours()] = [];
      }
      daySlots[h.getHours()].push(slot);
    }
    state.slots = newSlots;
  },
};

// TODO: slot module
// TODO: currentWishId module
export default {
  namespaced: true,
  state: {
    slot: null,
    currentWishId: null,
    selectedWishes: null,
  },
  getters: globalGetters,
  actions,
  mutations,
};
