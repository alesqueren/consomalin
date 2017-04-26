<template lang="pug">
  div
    div.notepad
      .redLine
      h2.title Mes rubriques
      Group(v-for="gid in gids" 
        v-bind:gid="gid"
        v-bind:key="gid")
      input#newGroup(v-model="newName" @keyup.enter="add" placeholder="Ajouter une rubrique", tabindex="1", @click.stop="")
      button.btn.btn-success.btn-sm.btn-create(v-if='creating' v-on:click="add")
        i.fa.fa-check.fa-xs
</template>

<script>
import Vue from 'vue';
import Group from './Group';

const $ = window.$;

export default {
  data() {
    return {
      newName: '',
    };
  },
  computed: {
    creating() {
      return this.$store.state.singleton.action.type === 'createGroup';
    },
    gids() {
      return this.$store.state.wishGroup.map(group => group.id);
    },
  },
  created() {
    if (!this.$store.state.singleton.activeGroupId) {
      this.$store.dispatch('singleton/set', {
        key: 'activeGroupId',
        value: this.gids[0],
      });
    }
  },
  methods: {
    add() {
      if (this.newName !== '') {
        this.$store.dispatch('wishGroup/addGroup', {
          name: this.newName,
        }).then((gid) => {
          this.$store.dispatch('singleton/set', {
            key: 'activeGroupId',
            value: gid,
          });
          Vue.nextTick(() => {
            $('#newWish').focus();
          });
        });
        this.newName = '';
      }
    },
  },
  watch: {
    newName(val) {
      if (val) {
        this.$store.dispatch('singleton/set', {
          key: 'action',
          value: {
            type: 'createGroup',
          },
        });
      } else {
        this.$store.dispatch('singleton/unset', {
          key: 'action',
        });
      }
    },
    creating(val) {
      if (!val) {
        this.newName = '';
      }
    },
  },
  components: { Group },
};
</script>
