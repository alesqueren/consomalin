<template lang="pug">
  div.root

    Basket(v-if="routeName !== 'help'")
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
        v-bind:class="{'prefered': currentWishResults && !currentWishResults[0]}"
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
              span Je valide mon panier

      //- tous les wishs sont matchés
      div#basketFull(v-else-if="matchedWishNb && matchedWishNb == selectedWishNb")
        router-link(:to='{ name: "withdraw" }')
            span.input-group-addon.nav-btn.prefered
              span Je valide mon panier

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
      router-link(:to='{ name: "ticket" }', v-if="selectedSlot")
        span.input-group-addon.nav-btn.prefered.specialContent
          span {{ confirmationMessage }}

      router-link(:to='{ name: "basket" }')
        span.input-group-addon.nav-btn
          span Revenir au panier

    div(v-if="routeName === 'ticket'")
      router-link(:to='{ name: "confirmation" }')
        span.input-group-addon.nav-btn.prefered.specialContent
          span Je valide ma commande de {{ total }}€ à {{ frenchTime }}
          
      router-link(:to='{ name: "withdraw" }')
        span.input-group-addon.nav-btn
          span Modifier mon horaire de retrait

      router-link(:to='{ name: "basket" }')
        span.input-group-addon.nav-btn
          span Revenir au panier

    div.contact-us(v-if="routeName === 'help'")
      h5 Une question, une suggestion ?
      span Contactez nous : contact@consomalin.ovh
</template>

<script>
import config from '../../../config';
import date from '../Utils/date';
import Basket from './Basket';

const $ = window.$;

export default {
  props: [],
  data() {
    return {
      demo: config.MODE_DEMO,
    };
  },
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
        const pds = this.$store.state.selection.basket[this.currentWish.gid][this.currentWish.id];
        return (Object.keys(pds).length !== 0);
      } catch (e) {
        return false;
      }
    },
    total() {
      return this.$store.getters['transaction/basketAmount'];
    },
    frenchTime() {
      const t = date.toFrenchTime(new Date(this.selectedSlot.dateTime));
      return t.hours + 'h' + t.minutes + ' le ' + t.dayName + ' ' + t.day + ' ' + t.monthName;
    },
    confirmationMessage() {
      if (this.frenchTime) {
        return 'Commander pour ' + this.frenchTime;
      }
      return 'Valider ma commande';
    },
    currentWish() {
      return this.$store.getters['sectionWishes/getCurrent'];
    },
    currentWishResults() {
      if (this.currentWish && this.currentWish.name) {
        return this.$store.state.product.searchs[this.currentWish.name];
      }
      return [];
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
  mounted() {
    if (this.demo) {
      $('#content > div.root').css('top', '+=50px');
    }
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
.contact-us{
  background-color: white;
  border: 1px solid #dedede;
  padding: 15px;
  border-radius: 4px;
}
.specialContent{
  white-space: inherit;
  padding: 13px;
  min-height: 47px;
  height: auto;
}
</style>
