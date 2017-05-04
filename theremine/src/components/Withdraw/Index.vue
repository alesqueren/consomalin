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
  a(href='/basket')
    button.btn.btn-primary.left(type="button") Revenir au panier
  a(href='#' @click="confirmSlot")
    button.btn.btn-success.right(type="button" v-bind:disabled="!selectedSlot") {{ confirmationMessage }}
</template>

<script>
import DayItem from './DayItem';

export default {
  computed: {
    days() {
      return this.$store.state.schedule.days;
    },
    selectedSlot() {
      return this.$store.state.singleton.selectedSlot;
    },
    isScheduleDataValid() {
      // TODO: check expiration > now()
      //       fetch schedule if data is expired
      return this.$store.state.schedule.expiration;
    },
    confirmationMessage() {
      if (this.selectedSlot) {
        const time = new Date(this.selectedSlot.dateTime);
        const day = (time.getDay() < 10) ? ('0' + time.getDay()) : time.getDay();
        const month = (time.getMonth() + 1 < 10) ? ('0' + (time.getMonth() + 1)) : time.getMonth() + 1;
        const minute = (time.getMinutes() < 10) ? ('0' + time.getMinutes()) : time.getMinutes();
        const hour = (time.getHours() < 10) ? ('0' + time.getHours()) : time.getHours();
        const frenchTime = hour + 'h' + minute + ' le ' + day + '/' + month;
        return 'Valider ma commande pour ' + frenchTime;
      }
      return 'Valider ma commande';
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
.row.no-gutters{
  border: 1px solid grey;
  padding: 15px;
  margin-bottom: 15px;
}
</style>
