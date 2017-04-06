import resources from '../resources';

const actions = {
  fetch({ commit }) {
    return new Promise((resolve) => {
      resources.schedule.get().then((response) => {
        const slots = JSON.parse(response.body);
        commit('set', { slots });
        resolve();
      });
    });
  },

  selectOne({ rootState, commit }, { slotId }) {
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
        commit('selectOne', { slotId });
        resolve(frenchTime);
      });
    });
  },

  setSlot({ commit }, { slotId }) {
    return new Promise((resolve) => {
      resources.slot.save({}, slotId).then(() => {
        commit('singleton/set', { key: 'slotId', value: slotId }, { root: true });
        resolve();
      });
    });
  },

};

const mutations = {

  selectOne: (state, slotId) => {
    for (let i = 0; i < state.length; i++) {
      const day = state[i];
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

  set: (state, { slots }) => {
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
    // TODO: use Vue.set ?
    state = daySlots;
  },

};

export default {
  namespaced: true,
  state: {},
  actions,
  mutations,
};
