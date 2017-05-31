<template lang="pug">
div#confirmation
  div.block
    h2 Votre commande à été transférée à Auchan.
    p Réglez et retirez vos produits à <strong>{{ frenchTime }}</strong>
    div.address
      div.left
        div Auchan Drive Balma
        div 70 Chemin de Gabardie
        div 31075 Toulouse
      div.left
        gmap-map(:center='center', :zoom='16', style='width: 500px; height: 300px')
          gmap-marker(v-for='m in markers', :position='m.position', :clickable='true', :draggable='true', @click='center=m.position' v-bind:key='m')
      div.right
        p Pour toute question concernant la commande&nbsp;: <br/>
         span S.A.V. Auchan Balma : 05 61 26 73 00
        p Pour toute question concernant le site Consomalin&nbsp;: <br/>
          span contact@consomalin.ovh

  div.block.help
    p Consomalin est encore jeune, aidez-nous à le faire grandir !
      a(href="https://goo.gl/forms/fso29Uz3ItSfkNAl1" target="blank")
        span.input-group-addon.nav-btn.prefered
          span Repondre au questionnaire

</template>

<script>
import * as VueGoogleMaps from 'vue2-google-maps';
import Vue from 'vue';
import date from '../Utils/date';

Vue.use(VueGoogleMaps, {
  load: {
    key: 'AIzaSyDJN1J8461pgYPXIsGGS3xANUWMC1XEUNs',
  },
});

export default {
  data() {
    return {
      center: { lat: 43.6340088, lng: 1.482934900000032 },
      markers: [{
        position: { lat: 43.6340088, lng: 1.482934900000032 },
      }],
    };
  },
  computed: {
    selectedSlot() {
      return this.$store.state.singleton.selectedSlot;
    },
    frenchTime() {
      const t = date.toFrenchTime(new Date(this.selectedSlot.dateTime));
      return t.hours + 'h' + t.minutes + ' le ' + t.dayName + ' ' + t.day + ' ' + t.monthName + ' (' + t.year + ')';
    },
  },
  mounted() {
  },
  methods: {
  },
  components: {},
};
</script>

<style scoped>
#confirmation{
  padding: 65px;
  font-size: 1.2em;
}
.block{
  border: 1px solid #dedede;
  padding: 30px;
  margin: 30px;
  min-width: 100%!important;
  background-color: white;
}
h2{
  text-align: center;
}
a{
  width: 250px;
  position: absolute;
  left:50%;
  margin-left: -125px;
  margin-top: 25px;
}
.address{
  display: table;
  width: 100%;
}
.left{
  display: table-cell;
  width: 25%;
  max-width: 165px;
}
.right{
  display: table-cell;
  width: 33%;
}
.help{
  height: 165px;
  text-align: center;
}
</style>
