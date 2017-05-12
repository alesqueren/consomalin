<template lang="pug">
  .wish.line(v-if="productIds.length > 0")
    span.product-name {{ wish.name }}
</template>

<script>

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
  },
};
</script>

<style scoped>
.wish{
  padding-left: 5px;
  cursor: pointer;
  border-bottom: 1px dotted #72c4ff;
}
.filled{
  text-decoration: line-through;
/*  text-decoration: underline;
  text-decoration: overline;*/
}
.active{
  color: green;
}
</style>
