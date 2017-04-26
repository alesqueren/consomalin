<template lang="pug">
  div
    div.notepad
      .redLine
      h2.title Mes rubriques
      Group(v-for="gid in gids" 
        v-bind:gid="gid"
        v-bind:key="gid")
      input#newGroup(v-model="newGroupName" v-on:keyup.enter="addWishGroup" placeholder="Ajouter une liste", tabindex="1")
</template>

<script>
import Vue from 'vue';
import Group from './Group';

const $ = window.$;

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
    if (!this.$store.state.singleton.activeGroupId) {
      this.$store.dispatch('singleton/set', { activeGroupId: this.gids[0] });
    }
  },
  methods: {
    addWishGroup() {
      if (this.newGroupName !== '') {
        this.$store.dispatch('wishGroup/addGroup', {
          name: this.newGroupName,
        }).then((gid) => {
          this.$store.dispatch('singleton/set', { activeGroupId: gid });
          Vue.nextTick(() => {
            $('#newWish').focus();
          });
        });
        this.newGroupName = '';
      }
    },
  },
  components: { Group },
};
</script>
