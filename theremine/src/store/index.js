import Vue from 'vue';
import Vuex from 'vuex';
import resources from '../resources';
import user from './user';
import wishlist from './wishlist/index';
import basket from './basket/index';

Vue.use(Vuex);

function getFirstUnmatchedSelectedWish(currBasket) {
  for (let i = 0; i < currBasket.length; i++) {
    const wish = currBasket[i];
    if (!wish.product.id) {
      return { gid: wish.gid, wid: wish.id };
    }
  }
  return null;
}

const globalGetters = {
  isEditing: state => id =>
    Boolean(state.inlineEdition === id),

  getCurrentWish: (state, getters) => {
    const currentWishId = state.basket.currentWishId;
    if (currentWishId) {
      return getters['wishlist/group/getByWish'](currentWishId);
    }
    return null;
  },

  getProduct: state => pid =>
    state.productInfos[pid],

};

const actions = {

  nextCurrentWish({ dispatch, getters, commit, state }) {
    if (getters['basket/getBasket']) {
      const newCurrentWish = getFirstUnmatchedSelectedWish(getters['basket/getBasket']);
      if (newCurrentWish) {
        const wish = getters['wishlist/group/getByWish'](newCurrentWish.wid);
        const gid = wish.gid;
        const wid = wish.id;
        resources.currentWish.save({}, { gid, wid }).then(() => {
          commit('basket/setCurrentWish', { gid, wid });
          const currentWish = getters['wishlist/group/getByWish'](wid);
          if (currentWish.name && !state.searchs[currentWish.name]) {
            dispatch('searchProductsWithName', { name: currentWish.name });
          }
        });
      } else {
        dispatch('removeCurrentWish');
      }
    }
  },

  removeCurrentWish({ commit }) {
    commit('basket/removeCurrentWish');
    resources.currentWish.delete();
  },

  setInlineEdition: ({ commit }, id) => {
    commit('setInlineEdition', { id });
  },

  searchProductsWithName: ({ commit, state }, { name }) => {
    if (!state.searchs[name]) {
      const uri = 'search?s=' + name;
      resources.products.get({ uri }, {}).then(({ body }) => {
        const products = JSON.parse(body);

        // todo: see array v-for
        commit('addSearchs', {
          name,
          products: Object.keys(products).reduce((acc, cur, i) => {
            acc[i] = cur;
            return acc;
          }, {}),
        });

        for (const pid in products) {
          commit('addProductInfos', {
            pid,
            infos: products[pid],
          });
        }
      });
    }
  },

  resetStore: ({ commit }) => {
    commit('resetStore');
  },

  updateProductInfos: ({ commit, rootState }, { pid, infos }) => {
    if (!rootState.productInfos[pid]) {
      commit('addProductInfos', { pid, infos });
    }
  },

  setSlots({ commit }) {
    return new Promise((resolve) => {
      resources.schedule.get().then((response) => {
        const body = JSON.parse(response.body);
        commit('setSlots', { slots: body.slots });
        resolve();
      });
    });
  },

  selectSlot({ commit, rootState }, { slotId }) {
    return new Promise((resolve) => {
      let date = '';
      let time = '';
      let frenchTime = '';
      const daySlots = rootState.slots;
      for (let i = 0; i < daySlots.length; i++) {
        const day = daySlots[i];
        for (let j = 0; j < day.slots.length; j++) {
          const slotHours = day.slots[j];
          if (slotHours) {
            for (let k = 0; k < slotHours.length; k++) {
              const slot = slotHours[k];
              if (slot.id === slotId) {
                date = slot.day;
                time = slot.time;
                const hours = parseInt(time.split(':')[0], 10);
                const minutes = time.split(':')[1];
                frenchTime = day.name + ' ' + hours + 'h' + minutes;
              }
            }
          }
        }
      }
      const dateTime = date + ' ' + time;
      resources.slot.save({}, { id: slotId, dateTime }).then(() => {
        commit('selectSlot', { slotId });
        resolve(frenchTime);
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
  resetStore: (state) => {
    Vue.set(state, 'wishGroups', null);
    Vue.set(state, 'currentBasket', null);
    Vue.set(state, 'searchs', null);
    Vue.set(state, 'productInfos', null);
  },

  setWishGroupsAndCurrentBasket(state, { wishGroups, currentBasket }) {
    return new Promise((resolve) => {
      state.wishlist.group.wishGroups = wishGroups;
      state.basket = currentBasket;
      resolve();
    });
  },

  setInlineEdition: (state, { id }) => {
    state.inlineEdition = id;
  },

  // TODO: move
  addSearchs: (state, { name, products }) => {
    Vue.set(state.searchs, name, products);
  },

  addProductInfos: (state, { pid, infos }) => {
    Vue.set(state.productInfos, pid, infos);
  },

  setMatchingProducts: (state, { wish, products }) => {
    for (let i = 0; i < state.wishGroups.length; i++) {
      const wishgroup = state.wishGroups[i];
      if (wishgroup.id === wish.gid) {
        for (let j = 0; j < wishgroup.wishes.length; j++) {
          const tmpWish = wishgroup.wishes[j];
          if (tmpWish.id === wish.id) {
            Vue.set(state.wishGroups[i].wishes[j], 'matchingProducts', products);
          }
        }
      }
    }
  },

  selectGroup: (state, { gid }) => {
    const selectWishes = {};
    for (const i in state.wishlist.group.wishGroups) {
      const group = state.wishlist.group.wishGroups[i];
      if (group.id === gid) {
        for (const j in group.wishes) {
          const wish = group.wishes[j];
          selectWishes[wish.id] = {};
        }
      }
    }
    Vue.set(state.basket.selectedWishes, gid, selectWishes);
  },

  selectSlot: (state, slotId) => {
    const daySlots = state.slots;
    for (let i = 0; i < daySlots.length; i++) {
      const day = daySlots[i];
      for (let j = 0; j < day.slots.length; j++) {
        const slotHours = day.slots[j];
        if (slotHours) {
          for (let k = 0; k < slotHours.length; k++) {
            const slot = slotHours[k];
            slot.selected = false;
            if (slot.id === slotId) {
              slot.selected = true;
            }
          }
        }
      }
    }
    state.currentBasket.slot = slotId;
  },

  setSlots: (state, { slots }) => {
    const days = ['Dimanche', 'Lundi', 'Mardi', 'Mercredi', 'Jeudi', 'Vendredi', 'Samedi'];
    const daysSlots = [];
    let daySlots = [];
    let precedentDay = new Date(slots[1].day + ' ' + slots[1].time).getHours();
    for (let i = 0; i < slots.length; i++) {
      const slot = slots[i];
      slot.selected = false;
      const currentDay = days[new Date(slot.day).getDay()];
      const currentHour = new Date(slot.day + ' ' + slot.time).getHours();
      const isLastSlot = i + 1 === slots.length;
      if (precedentDay !== currentDay || isLastSlot) {
        if (isLastSlot) {
          daySlots[currentHour].push(slot);
        }
        daysSlots.push({ name: currentDay, slots: daySlots });
        daySlots = [];
      }
      precedentDay = currentDay;
      if (!daySlots[currentHour]) {
        daySlots[currentHour] = [];
      }
      daySlots[currentHour].push(slot);
    }
  },
};


export default new Vuex.Store({
  strict: true,

  state: {
    searchs: {},
    productInfos: {},
    inlineEdition: null,
    slots: [],
  },

  getters: globalGetters,
  actions,
  mutations,

  modules: {
    user,
    wishlist,
    basket,
  },
});
