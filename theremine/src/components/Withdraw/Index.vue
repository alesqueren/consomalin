<template lang="pug">
div#slots
  h2 Horaires de retrait disponibles
  .border
  div.slots
    .container-fluid
      .row.no-gutters
        <i v-show="!isScheduleDataValid" class="text-center fa fa-spinner fa-spin fa-5x" style="width: 100%;"></i>
        <i v-show="!isScheduleDataValid" class="text-center" style="width: 100%;display: inline-block;">Nous chargeons les horaires de retraits disponibles</i>
        day-item(v-for="day in days" 
          v-bind:day="day" 
          v-bind:key="day"
          style="min-width:150px;")
        .scale
          .title Affluence :
          .min Faible
          .max Importante
          .colors

</template>

<script>
import DayItem from './DayItem';
import router from '../../router';

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
    selectedWishes() {
      return this.$store.getters['selection/getMatchedWishes'];
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
  created() {
    if (!Object.keys(this.selectedWishes).length) {
      router.push({ name: 'basket' });
    }
  },
  components: { DayItem },
};
</script>

<style scoped>
#slots{
  padding: 65px;
}
h2{
  text-align: center;
}
.border {
  position: absolute;
  content: '';
  background: var(--color2);
  height: 2px;
  width: 380px;
  left: 50%;
  margin-left: -180px;
  margin-top: 5px;
  margin-bottom: 40px;
}
.slots{
  margin-top: 55px;
}
.row.no-gutters{
  border: 1px solid grey;
  padding: 15px;
  margin-bottom: 15px;
}
.scale{
  position: relative;
  margin-top: 25px;
  width: 150px;
}
.scale .min{
  float: left;
}
.scale .max{
  float: right;
}
.scale .colors{
  clear: both;
  width: 150px;
  height: 25px;
  background: linear-gradient(45deg, rgb(50, 255, 0), rgb(255, 32, 0));
}
</style>
