<template lang='pug'>
.col(v-if='day.name')
  h2  {{day.name}}
  div(v-for='slotHours in day.slots', :key='slotHours')
    slot-item(v-for='pickupslot in slotHours', :key='pickupslot', v-bind:pickupslot='pickupslot')
</template>
<script>
export default {
  props: ['day'],
  data() {
    return {
      textNext: 'Valider mon horaire',
      disableNext: true,
    };
  },
  methods: {
    selectSlot: (slotId) => {
      let frenchTime = '';
      this.disableNext = false;
      for (let i = 0; i < this.daySlots.length; i++) {
        const day = this.daySlots[i];
        for (let j = 0; j < day.slots.length; j++) {
          const slotHours = day.slots[j];
          if (slotHours) {
            for (let k = 0; k < slotHours.length; k++) {
              const slot = slotHours[k];
              slot.selected = false;
              if (slot.id === slotId) {
                slot.selected = true;
                const time = slot.time;
                frenchTime = day.name + ' ' + parseInt(time.split(':')[0], 10) + 'h' + time.split(':')[1];
              }
            }
          }
        }
      }
      this.textNext = 'Valider ma commande pour ' + frenchTime;
      this.$store.dispatch('selectSlot');
    },
  },
};
</script>;
