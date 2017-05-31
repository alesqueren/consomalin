<template lang="pug">
  div.wishgroup(v-if="matchedWishes.length")
    .groupName {{ name }}
    Wish(v-for="wid in matchedWishes" 
      v-bind:wid="wid" 
      v-bind:gid="gid" 
      v-bind:key="wid")
</template>

<script>
import Wish from './Wish';

export default {
  props: ['gid'],
  data() {
    return {
      newWishName: '',
    };
  },
  computed: {
    name() {
      return this.$store.getters['wishGroup/getGroup']({ gid: this.gid }).name;
    },
    matchedWishes() {
      return this.$store.getters['selection/getMatchedWishesByGroup']({ gid: this.gid });
    },
  },
  methods: {
  },
  components: { Wish },
};

</script>

<style scoped>
.groupName {
  font-size: 1em;
  font-weight: bold;
}
</style>
