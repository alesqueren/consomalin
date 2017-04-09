<template lang='pug'>
  div.col.main
    h2 Mes listes de courses:
    div#notepad
      .lines
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
#notepad{
/*  background-color: #f5f5f5;
  width: 600px;
  margin: 0 auto;
  padding: 0;*/
  position: relative;
  color: #555;
  font-size: 1.5em;
  padding: 0 !important;
  min-width: 250px;
  font-family: courier, monospace;
  border: 1px solid #dedede;
}
.lines {
  position: absolute;
  border-left: 1px solid #ffaa9f;
  border-right: 1px solid #ffaa9f;
  width: 4px;
  height: 100%;
  margin-left: 65px;
  z-index: 2;
}
#newGroup{
  font-family: gunny;
  border: none;
  font-size: 1.5em;
  height: 50px;
  line-height: 50px;
  padding-left: 92px;
}
textarea:focus, input:focus{
    outline: none;
}
</style>
