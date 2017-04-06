<template lang='pug'>
  div.product-item.list-group-item.col-md-2(@click="selectProduct()")
   img.product-img.center(v-bind:src="product.imageUrl")
   .name.center {{product.name}}
   .price <b>{{product.price}}&nbsp;€</b>
   .pu {{product.priceByQuantity}}&nbsp;€/u
</template>

<script>
export default {
  props: ['pid', 'maxProducts'],
  computed: {
    product() {
      return this.$store.state.product.details[this.pid];
    },
    currentWishId() {
      return this.$store.state.singleton.currentWishId;
    },
    currentWish() {
      return this.$store.getters['wishGroup/getWish'](this.currentWishId);
    },
  },
  methods: {
    selectProduct() {
      this.$store.dispatch('wishGroup/setWishProduct', {
        gid: this.currentWish.gid,
        wid: this.currentWish.id,
        pid: this.pid,
        quantity: 1,
      });
      this.$store.dispatch('currentWish/next');
    },
  },
};
</script>

<style scoped>
.product-img{
  width:100px;
  height:100px;
}
.price{
  font-size: 1.2em;
}
.pu{
  font-size: 0.7em;
}
.center{
  display: block;
  margin: 0 auto;
}
</style>
