<template lang="pug">
  div(v-if="currentWish")
    router-link(:to='{ name: "basket" }')
      span.input-group-addon.basket.grey-btn
        span Voir le panier

    // - current wish
    div
      div.titleCurrent
        span Produit en cours : {{currentWish.name}}
      div(v-if="productIds.length > 0")
        Wish(
          v-bind:wid="currentWish.id",
          v-bind:displayName="false"
          )
        div(style="clear:both")
      div.currentWishProducts(v-else)
        span Aucun produit selectionné
        
      div.next.input-group-addon.grey-btn(
        v-if="selectedWishNb !== matchedWishesLength && productIds.length > 0",
        @click="nextProduct"
        )
        span Produit suivant
        span.fa.fa-arrow-right.special-fa
      router-link(:to='{ name: "basket" }')
        div.next.input-group-addon.grey-btn(
          v-if="selectedWishNb === matchedWishesLength && productIds.length > 0")
          span Voir le panier
          span.fa.fa-arrow-right.special-fa

    // - previous wish
    div(v-if="previousProductIds.length > 0 && previousWid !== currentWish.id")
      div.titlePrevious
        span Dernier produit ajouté : {{previousWish.name}}
      div
        Wish(
          v-bind:wid="previousWid",
          v-bind:displayName="false"
          )
        div(style="clear:both")
</template>
<script>
import Wish from '../Basket/Wish';

export default {
  props: [],
  data() {
    return {
      delayTimer: null,
    };
  },
  computed: {
    currentWish() {
      return this.$store.getters['sectionWishes/getCurrent'];
    },
    selectedWishNb() {
      return this.$store.getters['selection/getOrderedSelectedWishes'].length;
    },
    matchedWishesLength() {
      return Object.keys(this.$store.getters['selection/getMatchedWishes']).length;
    },
    previousWish() {
      if (this.previousWid) {
        return this.$store.getters['wishGroup/getWish']({ wid: this.previousWid });
      }
      return null;
    },
    previousWid() {
      return this.$store.state.singleton && this.$store.state.singleton.previousWid;
    },
    // TODO: getter
    previousProductIds() {
      if (this.previousWid) {
        const previousWish = this.$store.state.selection[this.previousWish.gid][this.previousWid];
        return Object.keys(previousWish);
      }
      return 0;
    },
    productIds() {
      if (this.currentWish) {
        const wish = this.$store.state.selection[this.currentWish.gid][this.currentWish.id];
        return Object.keys(wish);
      }
      return 0;
    },
  },
  methods: {
    nextProduct() {
      this.$store.dispatch('sectionWishes/next');
    },
  },
  components: { Wish },
};
</script>

<style scoped>
.currentWishProducts {
  height: auto;
  min-width: 320px;
  width: 320px;
  padding: 5px;
  background-color: color(--white);
  border: 1px solid grey;
}
.titleCurrent {
  clear: both;
  font-size: 1.5em;
  padding-bottom: 10px;
}
.titlePrevious {
  clear: both;
  font-size: 1.5em;
  padding: 10px 0 10px 0;
}
.grey-btn{
  position: relative;
  width: 150px;
  height: 44px;
  float: right;
  cursor: pointer;
  text-align: center;
}
.grey-btn .special-fa{
  position: absolute;
  right: 10px;
  top: 12px;
}
.grey-btn:hover{
  background-color: #e6e6e6;
}
.basket{
  margin-bottom: 30px;
}
.next{
  margin: 30px 0 30px 0;
  background-color: #5BC0B2;
  border-radius: 3px;
}
.next:hover{
  background-color: #4AB080;
}
</style>
