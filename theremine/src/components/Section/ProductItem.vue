<template lang='pug'>
  div.product-item.list-group-item.col-md-2(@click="selectProduct()")
   img(style="width:75px;height:75px;" v-bind:src="product.imageUrl")
   span.name {{product.name}}
   span.price : {{product.price}}€
</template>

<script>
export default {
  props: ['product', 'productkey', 'currentWish'],
  methods: {
    selectProduct() {
      this.$store.dispatch('updateProductInfos',
        {
          pid: this.productkey,
          name: this.product.name,
          price: this.product.price,
          imageUrl: this.product.imageUrl,
        },
      );
      this.$store.dispatch('setProduct',
        {
          gid: this.currentWish.gid,
          wid: this.currentWish.id,
          pid: this.productkey,
          quantity: 1,
        },
      );
      this.$store.dispatch('nextCurrentWish');
    },
  },
};
</script>

<style scoped>
</style>
