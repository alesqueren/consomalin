<template lang="pug">
  div#list
    h2 Ma Liste
    Group(v-for="gid in selectedGroups" 
      v-bind:gid="gid" 
      v-bind:key="gid")
</template>

<script>
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
  components: { Group },
};

</script>

<style scoped>
#list{
  text-align: left;
  background-color: white;
  padding: 5px;
  border: 1px solid rgba(0,0,0,0.2);
  max-height: 100%;
  overflow-y: auto;
  overflow-x: hidden;
  width: 184px;
  position: absolute;
  left: -150px;
  top: 50px;
}
#list h2{
  text-align: center;
}
</style>
