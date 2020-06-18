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
      span.input-group-addon.nav-btn.prefered.specialContent.inactive(v-if="basketIsOrdering", @click.stop="order")
        span La commande est en cours de transfert chez Auchan Drive Balma ..
      span.input-group-addon.nav-btn.prefered.specialContent(v-if="basketIsPrepared && !basketIsOrdering", @click.stop="order")
        span Je valide ma commande de {{ total }}€ à {{ frenchTime }}
      span.input-group-addon.nav-btn.prefered.inactive(v-if="!basketIsPrepared")
        span Veuillez patienter..
          
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
import router from '../../router';

const $ = window.$;

export default {
  props: [],
  data() {
    return {
      demo: Boolean(config.demo !== 'false'),
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
    preparationDiff() {
      return this.$store.state.basket.preparationDiff;
    },
    basketIsPrepared() {
      return this.$store.state.basket.isBasketPrepared;
    },
    basketIsOrdering() {
      return this.$store.state.basket.isBasketOrdering;
    },
    total() {
      let res = this.$store.getters['transactions/basketAmount'];
      if (this.basketIsPrepared && this.preparationDiff.totalPrice) {
        res = this.preparationDiff.totalPrice;
      }
      return res;
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
    order() {
      this.$store.dispatch('basket/setIsBasketOrdering', true);
      this.$store.dispatch('basket/order').then(() => {
        this.$store.dispatch('basket/setIsBasketOrdering', false);
        this.$store.dispatch('basket/setIsBasketOrdered', true);
        router.push({ name: 'confirmation' });
      }, () => {
        router.push({ name: 'withdraw' });
      });
    },
  },
  mounted() {
    if (this.demo && window.innerWidth > 1200) {
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
@media screen and (max-width: 1200px) {
  .root {
    position: absolute;
    right: -290px;
    top: 31px;
  }
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
.inactive{
  cursor: default !important;
  color: black !important;
}
.inactive{
  cursor: default !important;
  color: grey !important;
}
</style>
