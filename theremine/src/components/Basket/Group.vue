<template lang="pug">
  div.wishgroup(v-if="selectedWishes.length")
    div
      strong.groupName(@click.stop="setActivation") {{ name }}
      SuperWish(v-for="wid in selectedWishes" 
        v-bind:wid="wid",
        v-bind:displayName="true",
        v-bind:key="wid",
        :data-wid="wid")
</template>

<script>
import router from '../../router';
import SuperWish from './SuperWish';

export default {
  props: ['gid'],
  computed: {
    name() {
      return this.$store.getters['wishGroup/getGroup']({ gid: this.gid }).name;
    },
    selectedWishes() {
      return this.$store.getters['selection/getSelectedWishesByGroup']({ gid: this.gid });
    },
  },
  methods: {
    setActivation() {
      this.$store.dispatch('singleton/set', { activeGroupId: this.gid });
      router.push({ name: 'wishlist' });
    },
  },
  components: { SuperWish },
};

</script>

<style scoped>
.wishgroup {
  position: relative;
  color: #555;
  clear: both;
  padding-top: 40px;
}
.groupName {
  cursor: pointer;
  font-family: learningCurve;
  color: black;
  font-size: 2em;
  text-align: left;
}
.groupName:hover {
  text-decoration: underline
}
</style>
