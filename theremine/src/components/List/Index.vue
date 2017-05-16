<template lang="pug">
  div#list
    .hole
    .hole
    .hole
    h2 Ma Liste
    .bottom.fa.fa-arrow-circle-down.fa-2x
    VuePerfectScrollbar#groups.groups
      Group(v-for="gid in selectedGroups" 
        v-bind:gid="gid" 
        v-bind:key="gid")
</template>

<script>
import VuePerfectScrollbar from 'vue-perfect-scrollbar';
import Group from './Group';

export default {
  props: [],
  data() {
    return {
      newWishName: '',
    };
  },
  computed: {
    selectedGroups() {
      return this.$store.state.wishGroup.map(group => group.id);
    },
  },
  components: { Group, VuePerfectScrollbar },
};

</script>

<style scoped>
#list{
  position: fixed;
  text-align: left;
  width: 184px;
  left: 0;
  top: 50px;
  font-size: 0.9em;
  background-color: white;
  overflow-y: auto;
  overflow-x: hidden;
  height: 100vh;
}
#list .groups{
  border-top: 1px solid red;
  width: 184px;
  overflow-y: auto;
  overflow-x: hidden;
  position: fixed;
  left: 0;
  top: 125px;
  background-color: white;
  padding-bottom: 10px;
  min-height: 86vh;
  max-height: 86vh;
  background-size: 100% 21px;
}
#list h2{
  text-align: center;
  position: fixed;
  left: 32px;
  top: 83px;
  font-family: learningCurve;
}
.hole{
  float: left;
  border: 1px solid var(--color2);
  border-radius: 15px;
  height: 20px;
  width: 20px;
  margin-left: 30px;
  margin-top: 2px;
  background: linear-gradient(0deg, var(--color2), var(--color2-tr));
}
.bottom{
  position: fixed;
  z-index: 2;
  bottom: 2px;
  left: 75px;
}
.hidden{
  visibility: hidden;
}
</style>
