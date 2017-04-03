<template lang='pug'>
  div.col.no-gutter.wishgroup.list-group-item.col-3
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
      return this.$store.getters.getWishGroup(this.gid).name;
    },
    selectedWishes() {
      return this.$store.getters.getSelectedWishes(this.gid);
    },
  },
  methods: {
    setActivation() {
      this.$store.dispatch('setWishGroupActivation', this.gid);
    },
    addWish() {
      this.$store.dispatch('addWish', {
        gid: this.gid,
        name: this.newWishName,
      });
      this.newWishName = '';
    },
  },
  components: { Wish },
};

</script>

<style scoped>
.wishgroup {
  position: relative;
  color: #00B7FF;
  width: 400px;
}
.groupName {
  font-size: 1.2em;
  color: black;
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
