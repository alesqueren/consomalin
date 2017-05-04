<template lang="pug">
  li.wish
    span.product-name(v-bind:class="{'filled' : productIds.length > 0}") {{ wish.name }}
</template>

<script>
import router from '../../router';

export default {
  props: ['wid', 'gid'],
  data() {
    return {
      editingId: 'summary-' + this.wid,
      editingName: null,
    };
  },
  computed: {
    wish() {
      return this.$store.getters['wishGroup/getWish']({ wid: this.wid });
    },
    productIds() {
      const products = this.$store.state.selection.basket[this.wish.gid][this.wish.id];
      return products.map(p => p.pid).reverse();
    },
  },
};
</script>

<style scoped>
.wish{
  margin-left: 50px;
}
.filled{
  text-decoration: line-through;
}
</style>
