<template lang='pug'>
  div#wishlist.container-fluid
    h2 Mon panier: {{ matchedWishes }} articles, {{ total }} €
    router-link(:to='{ name: "withdraw" }')
      button.btn.btn-success.right(type="button") Passer au retrait
    div.col.main
      div.row.no-gutter.masonry
        Group.item(v-for="gid in selectedGroups" 
          v-bind:gid="gid"
          v-bind:key="gid")
</template>

<script>
import Group from './Group';

export default {
  computed: {
    selectedGroups() {
      return this.$store.getters.getSelectedWishGroups;
    },
    basket() {
      const basket = this.$store.getters.getBasket;
      return basket;
    },
    matchedWishes() {
      let matchedWishes = 0;
      for (let i = 0; i < this.basket.length; i += 1) {
        matchedWishes += this.basket[i].product.id ? 1 : 0;
      }
      return matchedWishes;
    },
    total() {
      return this.basket.reduce((prev, wish) => {
        if (wish.product && wish.product.infos && wish.product.infos.price) {
          const price = wish.product.infos.price;
          const priceProduct = price ? price * wish.product.quantity : 0;
          return prev + priceProduct;
        }
        return prev;
      }, 0).toFixed(2);
    },
  },
  components: { Group },
};
</script>
<style>
.masonry { /* Masonry container */
  column-count: 4;
  column-gap: 1em;
}
</style>
