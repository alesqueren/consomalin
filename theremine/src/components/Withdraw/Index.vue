<template lang='pug'>
div#slots
  <i v-show="!isScheduleDataValid" class="text-center fa fa-spinner fa-spin fa-5x" style="width: 100%;"></i>
  <i v-show="!isScheduleDataValid" class="text-center" style="width: 100%;display: inline-block;">Nous chargeons les horaires de retraits disponibles</i>
  div
    .container-fluid
      .row.no-gutters
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
        return 'Valider ma commande pour "' + this.selectedSlot.dateTime + '"';
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

// function rgb2hsv (pr,pg,pb) {
//   const r = pr / 255;
//   const g = pg / 255;
//   const b = pb / 255;
//   let h = 0;
//   let s = 0;
//   const v = Math.max(r, g, b);
//   const diff = v - Math.min(r, g, b);
//   const diffc = function(c){
//     return (v - c) / 6 / diff + 1 / 2;
//   };

//   if (diff === 0) {
//     h = s = 0;
//   } else {
//     s = diff / v;
//     const rr = diffc(r);
//     const gg = diffc(g);
//     const bb = diffc(b);

//     if (r === v) {
//       h = bb - gg;
//     } else if (g === v) {
//       h = ((1 / 3) + rr) - bb;
//     } else if (b === v) {
//       h = ((2 / 3) + gg) - rr;
//     }
//     if (h < 0) {
//       h += 1;
//     }else if (h > 1) {
//       h -= 1;
//     }
//   }
//   return {
//     h: Math.round(h * 360),
//     s: Math.round(s * 100),
//     v: Math.round(v * 100)
//   };
// }
</script>

<style scoped>
</style>
