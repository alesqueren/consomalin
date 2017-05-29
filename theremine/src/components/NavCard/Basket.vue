<template lang="pug">
    div.basket
      div.left-part
        div.basket-logo
          i.fa.fa-shopping-cart.fa-lg
      div.middle-part(v-bind:class="{ large: !matchedWishNb}" transition="fadeOut")
        span(v-if="selectedWishNb <= 1") {{selectedWishNb}} produit <br/>
        span(v-if="selectedWishNb > 1") {{selectedWishNb}} produits <br/>
        span(v-if="isBasketFull")
          b Panier rempli
          br
        span(v-else-if="matchedWishNb === 1" transition="fadeOut") {{matchedWishNb}} choisi<br/>
        span(v-else-if="matchedWishNb > 1" transition="fadeOut") {{matchedWishNb}} choisis<br/>
      div.right-part.total(v-if="matchedWishNb")
        span {{total}}&nbsp;€
</template>

<script>
export default {
  props: [],
  computed: {
    selectedWishNb() {
      return this.$store.getters['selection/getOrderedSelectedWishes'].length;
    },
    matchedWishNb() {
      return Object.keys(this.$store.getters['selection/getMatchedWishes']).length;
    },
    total() {
      return this.$store.getters['transaction/basketAmount'];
    },
    isBasketFull() {
      return this.matchedWishNb && this.matchedWishNb === this.selectedWishNb;
    },
  },
  methods: {
    nextProduct() {
      this.$store.dispatch('sectionWishes/next');
    },
  },
};
</script>

<style>
.basket{
  display: table;
  color: white;
  height: 60px;
  width: 320px;
  background-color: var(--color2);
  padding: 15px;
  margin-bottom: 15px;
}
.basket .left-part {
  display: table-cell;
  width: 42px;
  height: 42px;
}
.basket .middle-part {
  display: table-cell;
  width: 160px;
  vertical-align: middle;
}
.basket .middle-part span {
  display: block;
  position: relative;
  opacity: 1;
  text-align: center;
  font-size: 1.1em;
}
.basket .middle-part.large span {
  font-size: 1.5em;
}
.basket .right-part {
  display: table-cell;
  width: 70px;
  vertical-align: middle;
}
.basket .basket-logo {
  position: absolute;
  top: 17px;
  width: 39px;
  height: 39px;
  border-radius: 21px;
  border: 3px solid white;
}
.basket .basket-logo i {
  position: absolute;
  top: 10px;
  margin-left: 7px;
}
.basket .total {
  font-size: 2em;
}

/// CSS transition
.fadeOut-transition {
  visibility: visible;
  opacity: 1;
}

.fadeOut-enter,
.fadeOut-leave {
  opacity: 0;
}
</style>
