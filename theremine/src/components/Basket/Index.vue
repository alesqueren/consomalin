<template lang='pug'>
  div#wishlist.container-fluid
    h2 Mon panier:
    router-link(:to='{ name: "withdraw" }')
      button.btn.btn-success.right(type="button") Passer au retrait
    div.col.main
      div.row.no-gutter
        Group(v-for="gid in selectedGroups" 
          v-bind:gid="gid"
          v-bind:key="gid")
</template>

<script>
import Group from './Group';

export default {
  computed: {
    selectedGroups() {
      return this.$store.getters.getSelectedWishGroups;
    },
  },
  components: { Group },
};
</script>
