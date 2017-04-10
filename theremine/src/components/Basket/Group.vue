<template lang='pug'>
  div.col.no-gutter.wishgroup.list-group-item.col-3(v-if="selectedWishes.length")
    div
      span.groupName(@click.stop="setActivation") <strong>{{ name }}</strong>
      Wish(v-for="wid in selectedWishes" 
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
  width: 400px;
  min-width: 393px;
  background-color: #eee;
  display: inline-block;
  margin: 0 0 1em;
  width: 100%;
}
.groupName {
  font-family: gunny;
  color: black;
  font-size: 2.5em;
  text-align: center;
  width: 100%;
  display: block;
}
.topright {
  position: absolute;
  top: 0;
  right: 0;
}
.bottomright {
  position: absolute;
  bottom: 0;
  right: 0;
}
</style>
