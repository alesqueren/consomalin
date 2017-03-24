<template lang='pug'>
  div.product-item.list-group-item.col-md-2(@click="selectProduct()")
   img(style="width:75px;height:75px;" v-bind:src="product.imageUrl")
   span.name {{product.name}}
   span.price : {{product.price}}€
</template>

<script>
export default {
  props: ['currentwish'],
  methods: {
    selectProduct: () => {
      // var product = this.product;
      // this.$emit('select_product', this.productkey, product);
    },
  },
};
</script>

<style scoped>
</style>
