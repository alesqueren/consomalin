<template lang="pug">
  div#list
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
  text-align: right;
  background-color: white;
}
</style>
