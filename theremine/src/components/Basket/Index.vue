<template lang='pug'>
  div#wishlist.container-fluid
    h2 Mon panier: {{ matchedWishesLength }} articles, {{ total }} €
    router-link(:to='{ name: "withdraw" }')
      button.btn.btn-success.right(type="button") Passer au retrait
    div.col.main
      div.row.no-gutter.masonry
        Group.item(v-for="gid in selectedGroups" 
          v-bind:gid="gid"
          v-bind:key="gid")
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
    matchedWishesLength() {
      const matchedWishes = this.$store.getters.getMatchedWishes;
      return matchedWishes ? matchedWishes.length : 0;
    },
    total() {
      return this.$store.getters['transaction/basketAmount'];
    },
  },
  components: { Group },
};
</script>
<style>
.masonry { /* Masonry container */
  column-count: 4;
  column-gap: 1em;
}
</style>
