<template lang="pug">
  div#basketcard.right(v-if='username')
    div.left-part
      div.basket-logo
        i.fa.fa-shopping-basket
    div.right-part
      span.tooltip {{matchedWishesLength}} / {{selectedWishesNb}} produits<br/>
        span.tooltiptext.tooltip-bottom Vous avez encore {{remainingWishToMatch}} produits à choisir pour remplir votre panier
      span.tooltip Total : {{total}}&nbsp;€
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
      return this.$store.getters['selection/getOrdreredSelectedWishes'];
    },
    selectedWishesNb() {
      return this.$store.getters['selection/getOrdreredSelectedWishes'].length;
    },
    matchedWishesLength() {
      return Object.keys(this.$store.getters['selection/getMatchedWishes']).length;
    },
    remainingWishToMatch() {
      return this.selectedWishesNb - this.matchedWishesLength;
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
.tooltip {
    position: relative;
    opacity: 1;
}

.tooltip .tooltiptext {
  visibility: hidden;
  position: absolute;
  width: 120px;
  background-color: #555;
  color: #fff;
  text-align: center;
  padding: 5px 0;
  border-radius: 6px;
  z-index: 1;
  opacity: 0;
  transition: opacity 1s;
}

.tooltip-bottom{
  top: 135%;
  left: 50%;
  margin-left: -60px;
}

/* Tooltip arrow */
.tooltip .tooltip-bottom::after {
  content: "";
  position: absolute;
  bottom: 100%;
  left: 50%;
  margin-left: -5px;
  border-width: 5px;
  border-style: solid;
  border-color: transparent transparent #555 transparent;
}

.tooltip:hover .tooltiptext {
    visibility: visible;
    opacity: 1;
}

</style>
