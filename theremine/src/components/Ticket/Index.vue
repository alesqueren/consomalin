<template lang="pug">
div#ticket
  h2 Je valide mon ticket de caisse
  .box
    img.logo(src="../../assets/images/logo.jpg")
    .date {{ today }}
    Group.group(v-for="gid in selectedGroups" 
      v-bind:gid="gid" 
      v-bind:key="gid")
    .total
      .text TOTAL :
      .amount {{ total }}€
    .payment Réglement au retrait
    .thanks Merci de votre visite, à bientôt !
</template>

<script>
import Group from './Group';
import router from '../../router';

export default {
  computed: {
    selectedGroups() {
      return this.$store.state.wishGroup.map(group => group.id);
    },
    selectedWishes() {
      return this.$store.getters['selection/getMatchedWishes'];
    },
    today() {
      let today = new Date();
      let dd = today.getDate();
      let mm = today.getMonth() + 1;

      const yyyy = today.getFullYear();
      if (dd < 10) {
        dd = '0' + dd;
      }
      if (mm < 10) {
        mm = '0' + mm;
      }
      today = dd + '/' + mm + '/' + yyyy;
      return today;
    },
    total() {
      return this.$store.getters['transaction/basketAmount'];
    },
  },
  created() {
    if (!Object.keys(this.selectedWishes).length) {
      router.push({ name: 'basket' });
    }
  },
  components: { Group },
};
</script>

<style scoped>
#ticket{
  margin: 45px;
}
.box{
  font-family: receipt;
  position: absolute;
  left: 50%;
  margin-left: -250px;
  width: 500px;
  background-color: white;
  border: 1px dotted black;
  padding: 30px;
}
.date{
  text-align: center;
  margin-bottom: 15px;
}
.logo{
  margin: 30px 0 15px 160px;
}
.total{
  padding-top: 30px;
  clear: both;
  font-size: 1.2em;
  font-weight: bold;
  width: 320px;
  padding-left: 130px;
}
.total .text{
  float: left;
}
.total .amount{
  float: right;
}
.group{
  clear: both;
}
.thanks{
  margin-top: 30px;
  text-align: center;
}
</style>
