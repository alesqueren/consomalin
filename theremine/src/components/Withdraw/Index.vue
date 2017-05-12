<template lang="pug">
div#slots
  h2 Je choisis un horaire de retrait
  div
    .container-fluid
      .row.no-gutters
        <i v-show="!isScheduleDataValid" class="text-center fa fa-spinner fa-spin fa-5x" style="width: 100%;"></i>
        <i v-show="!isScheduleDataValid" class="text-center" style="width: 100%;display: inline-block;">Nous chargeons les horaires de retraits disponibles</i>
        day-item(v-for="day in days" 
          v-bind:day="day" 
          v-bind:key="day"
          style="min-width:150px;")
</template>

<script>
import DayItem from './DayItem';

export default {
  computed: {
    days() {
      return this.$store.state.schedule.days;
    },
    isScheduleDataValid() {
      // TODO: check expiration > now()
      //       fetch schedule if data is expired
      return this.$store.state.schedule.expiration;
    },
  },
  mounted() {
    this.$store.dispatch('schedule/fetch');
  },
  methods: {
    confirmSlot() {
      this.$store.dispatch('transaction/order');
    },
  },
  components: { DayItem },
};
</script>

<style scoped>
#slots{
  padding: 45px;
}
.row.no-gutters{
  border: 1px solid grey;
  padding: 15px;
  margin-bottom: 15px;
}
</style>
