<template lang='pug'>
  div.line(v-on:click.stop="select")
    input(type="checkbox" name="select" v-model="selected")
    input.edition(v-if='editing'
      ref="editinput" 
      v-model="editingName"
      v-on:keyup.enter="validEdition"
      v-on:keyup.esc="finishEdition"
      v-on:blur="finishEdition")
    button.btn.btn-success.btn-sm.btn-edition(v-if='editing' @click.stop="validEdition")
      i.fa.fa-check.fa-xs
    label.name(v-else for="select") {{ name }}
    div.confirmDeletion(v-if='deleting' @click.stop="remove" @keyup.esc="finishDeletion")
      span.btn.btn-danger Confirmer la suppression

    div.buttns(v-if='!editing')
      i.fa.fa-pencil.fa-xs.action.edit(@click.stop="startEdition")
      i.fa.fa-trash-o.fa-xs.action.delete(@click.stop="startDeletion")
</template>

<script>
import Vue from 'vue';

export default {
  props: ['gid', 'wid'],
  data() {
    return {
      editingName: null,
    };
  },
  computed: {
    name() {
      return this.$store.getters['wishGroup/getWish']({ wid: this.wid }).name;
    },
    selected() {
      try {
        return Boolean(this.$store.state.selection[this.gid][this.wid]);
      } catch (e) {
        return false;
      }
    },
    editing() {
      const actionnedEntity = this.$store.state.singleton.actionnedEntity;
      const actionnedEntityId = actionnedEntity.id;
      const action = actionnedEntity.action;
      return action === 'edit' && actionnedEntityId === this.wid;
    },
    deleting() {
      const actionnedEntity = this.$store.state.singleton.actionnedEntity;
      const actionnedEntityId = actionnedEntity.id;
      const action = actionnedEntity.action;
      return action === 'delete' && actionnedEntityId === this.wid;
    },
  },
  methods: {
    select() {
      this.$store.dispatch('selection/selectWish', {
        wid: this.wid,
        selected: !this.selected,
      });
    },
    focus() {
      this.$refs.editinput.focus();
    },
    startEdition() {
      this.editingName = this.name;
      this.$store.dispatch('singleton/set', {
        key: 'actionnedEntity',
        value: {
          action: 'edit',
          id: this.wid,
        },
      });
      Vue.nextTick(this.focus);
    },
    validEdition() {
      this.$store.dispatch('wishGroup/renameWish', {
        wid: this.wid,
        name: this.editingName,
      });
      this.finishEdition();
    },
    finishEdition() {
      this.editingName = null;
      this.$store.dispatch('singleton/unset', { key: 'actionnedEntity' });
    },
    startDeletion() {
      this.$store.dispatch('singleton/set', {
        key: 'actionnedEntity',
        value: {
          action: 'delete',
          id: this.wid,
        },
      });
    },
    finishDeletion() {
      this.$store.dispatch('singleton/unset', {
        key: 'actionnedEntity',
      });
    },
    remove() {
      this.$store.dispatch('wishGroup/removeWish', { wid: this.wid });
    },
  },
};
</script>

<style scoped>
.line .buttns {
  visibility: hidden;
  position: absolute;
  top: 2px;
  right: 5px;
}
</style>

