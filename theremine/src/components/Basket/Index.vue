<template lang='pug'>
  div#wishlist.container-fluid
    h2 Mon panier:
    router-link(:to='{ name: "withdraw" }')
      button.btn.btn-success.right(type="button") Passer au retrait
    div.col.main
      div.row.no-gutter
        Group(v-for="group in selectedGroups" 
          v-bind:group="group"
          v-bind:key="group")
        input(v-model="newGroupName" v-on:keyup.enter="addWishGroup" placeholder="Ajouter une liste")
</template>

<script>
import Group from './Group';

export default {
  computed: {
    selectedGroups() {
      return this.$store.getters.getSelectedWishGroups;
    },
  },
  mounted() {
    this.$store.dispatch('updateWishGroupsAndCurrentBasket');
  },
  components: {},
};
</script>
