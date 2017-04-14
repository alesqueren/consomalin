<template lang='pug'>
  div#wishlist
    Group(v-for="gid in selectedGroups" 
      v-bind:gid="gid"
      v-bind:key="gid")
    div#recap
      div#edit-wishlist
        router-link(:to='{ name: "wishlist" }')
          button.btn.btn-primary.btn-sm(type="button") Editer ma liste de course
      div
        //- tous les wishs ne sont pas encore matchés
        div#missingProduct(v-if="matchedWishesLength && matchedWishesLength < selectedWishesNb")
          div Vous avez choisi {{matchedWishesLength}} / {{selectedWishesNb}} produits de votre liste de course
          div Total du panier : <span style="font-size: 2em;">{{total}} €</span>
          router-link(:to='{ name: "section" }')
            div(style="margin: 10px 0 0 10px;")
              button.btn.btn-success(type="button") Continuer mes courses
          div#force-continue
            router-link(:to='{ name: "withdraw" }')
              button.btn.btn-warning.btn-sm(type="button") Passer au retrait 

        //- tous les wishs sont matchés
        div#basketFull(v-else-if="matchedWishesLength && matchedWishesLength == selectedWishesNb")
          div Total du panier : <span style="font-size: 2em;">{{total}} €</span>
          router-link(:to='{ name: "withdraw" }')
            button.btn.btn-success(type="button") Passer au retrait

        //-  aucun wish
        div#startBasket(v-else-if="!matchedWishesLength && !selectedWishesNb")
          router-link(:to='{ name: "wishlist" }')
            button.btn.btn-success(type="button") Commencer une liste

        //- aucun wishs matchés
        div#startWishlist(v-else-if="!matchedWishesLength")
          router-link(:to='{ name: "section" }')
            button.btn.btn-success(type="button") Commencer mes courses
</template>

<script>
import Group from './Group';

export default {
  computed: {
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
    total() {
      return this.$store.getters['transaction/basketAmount'];
    },
  },
  components: { Group },
};
</script>
<style>
#wishlist{
  position: relative;
  height: auto;
  font: 14px "Lucida Grande", Helvetica, Arial, sans-serif;
  padding-bottom: 300px;
}
#wishlist:after {
    content:"";
    clear:both;
    display:block;
}
button{
  cursor: pointer;
}
#recap{
  position: absolute;
  bottom: 0;
  left: 0;
  clear: both;
  background-color: #e5e5e5;
  width: 100%;
  height: 250px;
}
#missingProduct{
  position: relative;
  text-align: center;
  margin-top: 50px;
  margin-bottom: 50px;
}
#force-continue{
  margin-top: 25px;
}
#basketFull{
  position: relative;
  text-align: center;
  margin-top: 50px;
  margin-bottom: 50px;
}
#startBasket{
  position: relative;
  text-align: center;
  margin-top: 50px;
  margin-bottom: 50px;
}
#startWishlist{
  position: relative;
  text-align: center;
  margin-top: 50px;
  margin-bottom: 50px;
}
#edit-wishlist{
  position: absolute;
  left: 50px;
  top: 100px;
  z-index: 2;
}
#force-continue{
  position: absolute;
  right: 50px;
  top: 32px;
  z-index: 2;
}
</style>
