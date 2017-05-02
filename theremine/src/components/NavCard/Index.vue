<template lang="pug">
  div.root

    Basket
    div(v-if="routeName === 'wishlist'")
      router-link(:to='{ name: "section" }', v-if="matchedWishNb && matchedWishNb < selectedWishNb")
        span.input-group-addon.nav-btn.prefered
          span Continuer mes courses
      router-link(:to='{ name: "basket" }', v-else-if="selectedWishNb && matchedWishNb === selectedWishNb")
        span.input-group-addon.nav-btn.prefered
          span Voir le panier
      router-link(:to='{ name: "section" }', v-else-if="selectedWishNb")
        span.input-group-addon.nav-btn.prefered
          span Commencer mes courses

    div(v-if="routeName === 'section' && hasChoosenProduct")
      div.next.input-group-addon.nav-btn.prefered(
        v-if="unmatchedWishNb && remainingWishesToChoose.length > 0", 
        @click="nextProduct")
        span Produit suivant
      router-link(:to='{ name: "basket" }', v-else)
        div.next.input-group-addon.nav-btn.prefered
          span Voir le panier
          span.fa.fa-arrow-right.special-fa

    div(v-if="routeName === 'basket'")
      //- tous les wishs ne sont pas encore matchés
      div#missingProduct(v-if="matchedWishNb && matchedWishNb < selectedWishNb")
        router-link(:to='{ name: "section" }')
          span.input-group-addon.nav-btn.prefered
            span Continuer mes courses
        div#force-continue
          router-link(:to='{ name: "withdraw" }')
            span.input-group-addon.nav-btn
              span Passer au retrait

      //- tous les wishs sont matchés
      div#basketFull(v-else-if="matchedWishNb && matchedWishNb == selectedWishNb")
        router-link(:to='{ name: "withdraw" }')
            span.input-group-addon.nav-btn.prefered
              span Passer au retrait

      //-  aucun wish
      div#startBasket(v-else-if="!matchedWishNb && !selectedWishNb")
        router-link(:to='{ name: "wishlist" }')
          span.input-group-addon.nav-btn.prefered
            span Commencer une liste

      //- aucun wishs matchés
      div#startWishlist(v-else-if="!matchedWishNb")
        router-link(:to='{ name: "section" }')
        span.input-group-addon.nav-btn.prefered
          span Commencer mes courses
</template>

<script>
import Basket from './Basket';

export default {
  props: [],
  computed: {
    routeName() {
      return this.$store.state.route.name;
    },
    selectedWishNb() {
      return this.$store.getters['selection/getOrderedSelectedWishes'].length;
    },
    matchedWishNb() {
      return Object.keys(this.$store.getters['selection/getMatchedWishes']).length;
    },
    remainingWishesToChoose() {
      return this.$store.getters['sectionWishes/getOrder'];
    },
    unmatchedWishNb() {
      return this.selectedWishNb - this.matchedWishNb;
    },
    hasChoosenProduct() {
      try {
        const currentWish = this.$store.getters['sectionWishes/getCurrent'];
        const pds = this.$store.state.selection.basket[currentWish.gid][currentWish.id];
        return (Object.keys(pds).length !== 0);
      } catch (e) {
        return false;
      }
    },
  },
  methods: {
    nextProduct() {
      this.$store.dispatch('sectionWishes/next');
    },
  },
  components: { Basket },
};
</script>

<style scoped>
.root{
  position: fixed;
  top: 65px;
  right: 15px;
  width: 349px;
}
.nav-btn {
  position: relative;
  width: auto;
  height: 47px;
  cursor: pointer;
  text-align: center;
}
.nav-btn:hover{
  background-color: #e6e6e6;
}
.nav-btn .special-fa {
  position: absolute;
  right: 10px;
  top: 12px;
}
.nav-btn.prefered {
  color: var(--white);
  font-weight: bolder;
  background-color: var(--success);
}
.nav-btn.prefered:hover {
  background-color: var(--color3-3);
}
</style>
