<template lang="pug">
  div.root
    div.basket
      div.left-part
        div.basket-logo
          i.fa.fa-shopping-basket
      div.right-part
        span.tooltip {{matchedWishesLength}} / {{selectedWishesNb}} produits<br/>
          span.tooltiptext.tooltip-bottom Vous avez encore {{remainingWishToMatch}} produits à choisir pour remplir votre panier
        span Total : {{total}}&nbsp;€

    div(v-if="routeName === 'wishlist'")
      router-link(:to='{ name: "section" }', v-if="matchedWishesLength && matchedWishesLength <= selectedWishesNb")
        span.input-group-addon.grey-btn
          span Continuer mes courses
      router-link(:to='{ name: "section" }', v-else)
        span.input-group-addon.grey-btn
          span Commencer mes courses

    div(v-if="routeName === 'section'")
      //- SI aucun resultat
      div.next.input-group-addon.grey-btn(@click="nextProduct")
        span(v-if="selectedWishesNb === matchedWishesLength") Voir le panier
        span(v-else) Produit suivant
        span.fa.fa-arrow-right.special-fa
    div(v-if="routeName === 'basket'")
      //- tous les wishs ne sont pas encore matchés
      div#missingProduct(v-if="matchedWishesLength && matchedWishesLength < selectedWishesNb")
        router-link(:to='{ name: "section" }')
          span.input-group-addon.grey-btn
            span Continuer mes courses
        div#force-continue
          router-link(:to='{ name: "withdraw" }')
            span.input-group-addon.grey-btn
              span Passer au retrait

      //- tous les wishs sont matchés
      div#basketFull(v-else-if="matchedWishesLength && matchedWishesLength == selectedWishesNb")
        div Total du panier : <span style="font-size: 2em;">{{total}} €</span>
        router-link(:to='{ name: "withdraw" }')
            span.input-group-addon.grey-btn
              span Passer au retrait

      //-  aucun wish
      div#startBasket(v-else-if="!matchedWishesLength && !selectedWishesNb")
        router-link(:to='{ name: "wishlist" }')
          span.input-group-addon.grey-btn
            span Commencer une liste

      //- aucun wishs matchés
      div#startWishlist(v-else-if="!matchedWishesLength")
        router-link(:to='{ name: "section" }')
        span.input-group-addon.grey-btn
          span Commencer mes courses
</template>

<script>
export default {
  props: [],
  computed: {
    routeName() {
      return this.$store.state.route.name;
    },
    selectedWishesNb() {
      return this.$store.getters['selection/getOrderedSelectedWishes'].length;
    },
    matchedWishesLength() {
      return Object.keys(this.$store.getters['selection/getMatchedWishes']).length;
    },
    remainingWishToMatch() {
      return this.selectedWishesNb - this.matchedWishesLength;
    },
    currentWish() {
      return this.$store.getters['sectionWishes/getCurrent'];
    },
    total() {
      return this.$store.getters['transaction/basketAmount'];
    },
  },
  methods: {
    nextProduct() {
      this.$store.dispatch('sectionWishes/next');
    },
  },
};

</script>

<style scoped>
.root{
  position: fixed;
  top: 59px;
  right: 50px;
  width: 320px;
}
.basket{
  display: table;
  color: black;
  height: 60px;
  width: 100%;
  padding-left: 60px;
}
.basket .left-part {
  display: table-cell;
  width: 70px;
  max-width: 70px;
}
.basket .right-part {
  display: table-cell;
  width: 150px;
  max-width: 150px;
  line-height: 30px;
  padding-top: 10px;
}
.basket .basket-logo {
  position: absolute;
  top: 15px;
  width: 35px;
  height: 35px;
  border-radius: 21px;
  border: 1px solid black;
}
.basket .basket-logo i {
  position: absolute;
  top: 9px;
  margin-left: 9px;
}
.grey-btn{
  position: relative;
  width: auto;
  height: 47px;
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
</style>
