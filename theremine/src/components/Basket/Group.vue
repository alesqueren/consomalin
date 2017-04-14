<template lang='pug'>
  div.wishgroup(v-if="selectedWishes.length")
    span.groupName(@click.stop="setActivation") <strong>{{ name }}</strong>
    Wish(v-for="wid in selectedWishes" 
      v-bind:wid="wid" 
      v-bind:gid="gid" 
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
@font-face {
    font-family: gunny;
    src: url('/static/fonts/gnyrwn971.ttf');
}
.wishgroup {
  position: relative;
  float: left;
  width: 350px;
  max-width: 350px;
  min-width: 350px;
  background-color: #eee;
  color: #555;
}
.groupName {
  cursor: pointer;
  font-family: gunny;
  color: black;
  font-size: 2.5em;
  text-align: center;
  width: 100%;
  display: block;
}
.groupName:hover {
  text-decoration: underline
}
</style>
