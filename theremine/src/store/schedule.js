import Vue from 'vue';
import resources from '../resources';

const dayNames = ['Dimanche', 'Lundi', 'Mardi', 'Mercredi', 'Jeudi', 'Vendredi', 'Samedi'];

const byDay = (slots) => {
  const res = [];
  let currentDay = {};
  let currentHour = {};

  for (let i = 0; i < slots.length; i++) {
    const slot = slots[i];
    // change day
    const name = dayNames[new Date(slot.day).getDay()];
    if (name !== currentDay.name) {
      currentDay = { name, hours: [] };
      res.push(currentDay);
    }
    // change hour
    const hour = new Date(slot.day + ' ' + slot.time).getHours();
    if (hour !== currentHour.hour) {
      currentHour = { hour, slots: [] };
      currentDay.hours.push(currentHour);
    }
    currentHour.slots.push(slot);
  }
  return res;
};

const actions = {
  fetch({ commit }) {
    return new Promise((resolve) => {
      resources.schedule.get().then(({ body }) => {
        const parsedBody = JSON.parse(body);
        commit('set', {
          schedule: byDay(parsedBody.slots),
          expiration: parsedBody.expiration,
        });
        resolve();
      });
    });
  },

  selectSlot({ commit }, { slotId, dateTime }) {
    const value = { id: slotId, dateTime };
    resources.slot.save({}, value).then(() => {
      commit('singleton/set', { selectedSlot: value }, { root: true });
    });
  },
};

const mutations = {
  set: (state, { slots, schedule, expiration }) => {
    Vue.set(state, 'days', schedule);
    Vue.set(state, 'slots', slots);
    Vue.set(state, 'expiration', expiration);
  },
};

export default {
  namespaced: true,
  state: {},
  actions,
  mutations,
};
