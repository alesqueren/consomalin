<template lang='pug'>
  div.col.main
    h2 Mes listes :
    div.row.no-gutter
      Group(v-for="group in wishlist" 
        v-bind:group="group"
        v-bind:key="group")
      input(v-model="newGroupName" v-on:keyup.enter="addWishGroup" placeholder="Ajouter une liste")
</template>

<script>
import Group from './Group';

export default {
  data() {
    return {
      newGroupName: '',
    };
  },
  computed: {
    wishlist() {
      return this.$store.getters.getWishlist;
    },
  },
  methods: {
    addWishGroup() {
      this.$store.dispatch('addWishGroup', this.newGroupName);
      this.newGroupName = '';
    },
  },
  mounted() {
    this.$store.dispatch('updateWishGroupsAndCurrentBasket');
  },
  components: { Group },
};
</script>
