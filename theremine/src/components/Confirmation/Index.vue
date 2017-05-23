<template lang="pug">
div#confirmation
  div.block
    h2 Votre commande à été transférée à Auchan.
    p Réglez et retirez vos produits à <strong>{{ frenchTime }}</strong>
    div.address
      div.left
        div Espace Gramont
        div 2 Chemin de Gabardi
        div 31000 Toulouse
      div.right
          div La carte
  div.block
    p Pour toute question concernant la commande : 05 61 26 73 00
    p Pour toute question concernant le site Consomalin : contact@consomalin.ovh

  div.block
    p Consomalin est encore jeune, aidez-nous à le faire grandir !
      a(href="https://goo.gl/forms/fso29Uz3ItSfkNAl1" target="blank")
        span.input-group-addon.nav-btn.prefered
          span Repondre au questionnaire

</template>

<script>
import date from '../Utils/date';

export default {
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
  background-color: white;
}
h2{
  text-align: center;
}
a{
  width: 250px;
}
.address{
  display: table;
  width: 100%;
}
.left{
  display: table-cell;
  width: 50%;
}
.right{
  display: table-cell;
  width: 50%;
  text-align: center;
}
</style>
