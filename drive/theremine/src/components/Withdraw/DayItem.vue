<template lang="pug">
.col(v-if='day.name')
  h2 {{day.name}}
  h5 {{day.date}}
  div.slotHours(v-for='hour in day.hours', :key='hour')
    slot-item(v-for='pickupSlot in hour.slots', 
      v-bind:pickupSlot="pickupSlot", 
      :key='pickupSlot.id')
</template>

<script>
import SlotItem from './SlotItem';

export default {
  props: ['day'],
  components: { SlotItem },
};
</script>;

<style scoped>
.slotHours{
  clear:both;
}
</style>
