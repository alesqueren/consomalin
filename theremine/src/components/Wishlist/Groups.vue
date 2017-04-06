<template lang='pug'>
  div.col.main
    h2 Mes listes :
    div.row.no-gutter
      Group(v-for="gid in gids" 
        v-bind:gid="gid"
        v-bind:key="gid")
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
    gids() {
      return this.$store.state.selection.getSelectedGroups;
    },
  },
  methods: {
    addWishGroup() {
      this.$store.dispatch('wishGroup/addGroup', this.newGroupName);
      this.newGroupName = '';
    },
  },
  components: { Group },
};
</script>
