<template lang="pug">
  .products
    Product(v-for="pid in productIds"
      v-if="!productHasMultipleWish(pid)"
      v-bind:pid="pid" 
      v-bind:key="pid")
</template>

<script>
import Product from './Product';

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
  methods: {
    productHasMultipleWish(pid) {
      if (this.$store.getters['product/getWishesAssociate']({ pid }).length > 1) {
        this.$store.dispatch('product/addProductInMultipleWish', { pid });
        return true;
      }
      return false;
    },
  },
  components: { Product },
};
</script>

<style scoped>
.wishName {
  font-size: 0.9em;
}
</style>
