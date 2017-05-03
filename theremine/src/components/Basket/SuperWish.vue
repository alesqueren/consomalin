<template lang="pug">
  div(v-if="productIds.length <= 1")
      Wish(
        v-bind:wid="wid",
        v-bind:key="wid")
  div(v-else)
    div(v-for="(pid, index) in productIds")
      Wish(
        v-bind:wid="wid",
        v-bind:detailProduct="index + 1",
        v-bind:pid="pid",
        v-bind:key="wid")
</template>

<script>
import Wish from './Wish';

export default {
  props: ['wid'],
  computed: {
    wish() {
      return this.$store.getters['wishGroup/getWish']({ wid: this.wid });
    },
    productIds() {
      const products = this.$store.state.selection.basket[this.wish.gid][this.wish.id];
      return products.map(p => p.pid).reverse();
    },
  },
  components: { Wish },
};

</script>
