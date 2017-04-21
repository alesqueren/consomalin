<template lang="pug">
  div.wishgroup(v-if="selectedWishes.length")
    div.groupName(@click.stop="setActivation") <strong>{{ name }}</strong>
    Wish(v-for="wid in selectedWishes" 
      v-bind:wid="wid",
      v-bind:displayName="true",
      v-bind:key="wid")
</template>

<script>
import router from '../../router';
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
    selectedWishes() {
      return this.$store.getters['selection/getSelectedWishesByGroup']({ gid: this.gid });
    },
  },
  methods: {
    setActivation() {
      this.$store.dispatch('singleton/set', {
        key: 'activeGroupId',
        value: this.gid,
      });
      router.push({ name: 'wishlist' });
    },
  },
  components: { Wish },
};

</script>

<style scoped>
.wishgroup {
  position: relative;
  color: #555;
  clear: both;
}
.groupName {
  cursor: pointer;
  font-family: gunny;
  color: black;
  font-size: 2.5em;
  text-align: center;
}
.groupName:hover {
  text-decoration: underline
}
</style>
