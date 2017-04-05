<template lang='pug'>
.col(v-if='day.name')
  h2  {{day.name}}
  div.slotHours(v-for='slotHours in day.slots', :key='slotHours')
    slot-item(v-for='pickupslot in slotHours', v-bind:pickupslot="pickupslot", :key='pickupslot')
</template>
<script>
import SlotItem from './SlotItem';

export default {
  props: ['day'],
  data() {
    return {
      textNext: 'Valider mon horaire',
      disableNext: true,
    };
  },
  methods: {
  },
  components: { SlotItem },
};
</script>;
<style>
.slotHours{
  clear:both;
}
</style>
