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

    div(v-if="routeName === 'section'")
      div.next.input-group-addon.nav-btn.prefered(
        v-if="hasChoosenProduct && remainingWishesToChoose.length > 0",
        @click="nextProduct")
        span Produit suivant
      router-link(
          v-else-if="!hasChoosenProduct && !hasCurrentWish && selectedWishNb === 0",
          :to='{ name: "wishlist" }')
        div.next.input-group-addon.nav-btn.prefered
          span Gérer ma liste de course
      router-link(
          v-else-if="hasChoosenProduct || !hasCurrentWish",
          :to='{ name: "basket" }')
        div.next.input-group-addon.nav-btn.prefered
          span Voir le panier
      div.next.input-group-addon.nav-btn(
        v-else="!hasChoosenProduct && hasCurrentWish",
        @click="erase")
        span Je n'en veux plus

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

    div(v-if="routeName === 'withdraw'")
      router-link(:to='{ name: "basket" }')
        span.input-group-addon.nav-btn
          span Revenir au panier

      router-link(:to='{ name: "confirmation" }', v-if="selectedSlot")
        span.input-group-addon.nav-btn.prefered
          span {{ confirmationMessage }}
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
    hasCurrentWish() {
      return Boolean(this.$store.getters['sectionWishes/getCurrent']);
    },
    selectedSlot() {
      return this.$store.state.singleton.selectedSlot;
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
    confirmationMessage() {
      if (this.selectedSlot) {
        const time = new Date(this.selectedSlot.dateTime);
        const day = (time.getDay() < 10) ? ('0' + time.getDay()) : time.getDay();
        const month = (time.getMonth() + 1 < 10) ? ('0' + (time.getMonth() + 1)) : time.getMonth() + 1;
        const minute = (time.getMinutes() < 10) ? ('0' + time.getMinutes()) : time.getMinutes();
        const hour = (time.getHours() < 10) ? ('0' + time.getHours()) : time.getHours();
        const frenchTime = hour + 'h' + minute + ' le ' + day + '/' + month;
        return 'Commander pour ' + frenchTime;
      }
      return 'Valider ma commande';
    },
  },
  methods: {
    nextProduct() {
      this.$store.dispatch('sectionWishes/next');
    },
    erase() {
      const currentWish = this.$store.getters['sectionWishes/getCurrent'];
      const wid = currentWish.id;
      const selected = false;
      this.$store.dispatch('selection/selectWish', { wid, selected }).then(() => {
        this.nextProduct();
      });
    },
  },
  components: { Basket },
};
</script>

<style scoped>
.root{
  position: fixed;
  top: 80px;
  right: 30px;
  width: 320px;
}
a{
    margin-top: 15px;
}
</style>
