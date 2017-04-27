<template lang="pug">
  div#basketcard.right(v-if='username')
    div.left-part
      div.basket-logo
        i.fa.fa-shopping-basket
    div.right-part
      span.tooltip {{matchedWishesLength}} / {{selectedWishNb}} produits<br/>
        span.tooltiptext.tooltip-bottom Vous avez encore {{remainingWishToMatch}} produits à choisir pour remplir votre panier
      span Total : {{total}}&nbsp;€
</template>

<script>

export default {
  computed: {
    username() {
      return this.$store.state.user.username;
    },
    selectedGroups() {
      return this.$store.state.wishGroup.map(group => group.id);
    },
    basket() {
      return this.$store.getters['selection/getOrderedSelectedWishes'];
    },
    selectedWishNb() {
      return this.$store.getters['selection/getOrderedSelectedWishes'].length;
    },
    matchedWishesLength() {
      return Object.keys(this.$store.getters['selection/getMatchedWishes']).length;
    },
    remainingWishToMatch() {
      return this.selectedWishNb - this.matchedWishesLength;
    },
    total() {
      return this.$store.getters['transaction/basketAmount'];
    },
  },
};
</script>
<style>
#basketcard{
  display: table;
  color: white;
  height: 60px;
  width: 185px;
}
#basketcard .left-part {
  display: table-cell;
  width: 70px;
  max-width: 70px;
}
#basketcard .right-part {
  display: table-cell;
  width: 150px;
  max-width: 150px;
  line-height: 30px;
  padding-top: 10px;
}
#basketcard .basket-logo {
  position: absolute;
  top: 15px;
  width: 35px;
  height: 35px;
  border-radius: 21px;
  border: 1px solid black;
}
#basketcard .basket-logo i {
  position: absolute;
  top: 9px;
  margin-left: 9px;
}

</style>
