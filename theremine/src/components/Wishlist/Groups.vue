<template lang='pug'>
  div.col.main
    div.notepad
      .redLine
      h2.title Mes listes de courses
      Group(v-for="gid in gids" 
        v-bind:gid="gid"
        v-bind:key="gid")
      input#newGroup(v-model="newGroupName" v-on:keyup.enter="addWishGroup" placeholder="Ajouter une liste")
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
      return this.$store.state.wishGroup.map(group => group.id);
    },
  },
  created() {
    this.$store.dispatch('singleton/set', {
      key: 'activeGroupId',
      value: this.gids[0],
    });
  },
  methods: {
    addWishGroup() {
      this.$store.dispatch('wishGroup/addGroup', { name: this.newGroupName });
      this.newGroupName = '';
    },
  },
  components: { Group },
};
</script>
<style>
</style>
