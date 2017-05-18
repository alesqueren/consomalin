<template lang="pug">
  div.product(
    @click='select()'
    v-if="productInfos")
    div.productName {{productInfos.name}}
    span.total &nbsp;&nbsp;&nbsp;&nbsp;{{total}}€

</template>

<script>

export default {
  props: ['wid', 'pid'],
  computed: {
    quantity() {
      const wish = this.$store.getters['wishGroup/getWish']({ wid: this.wid });
      const product = this.$store.getters['selection/getProduct']({
        gid: wish.gid,
        wid: this.wid,
        pid: this.pid,
      });
      return product.quantity;
    },
    productInfos() {
      return this.$store.state.product.details[this.pid];
    },
    total() {
      const total = this.productInfos.price * this.quantity;
      return parseFloat(total).toFixed(2);
    },
  },
};
</script>

<style scoped>
.product {
}
.productName {
  float: left;
  width: 340px;
}
.total {
  float: right;
}
</style>
