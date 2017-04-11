<template lang='pug'>
  div.line(v-on:click.stop="select")
    input(type="checkbox" name="select" v-model="selected")
    input(v-if='editing'
      ref="editinput" 
      v-model="editingName"
      v-on:keyup.enter="validEdition"
      v-on:keyup.esc="finishEdition"
      v-on:blur="finishEdition")
    button.btn.btn-success.btn-sm(v-if='editing' @click.stop="validEdition")
      i.fa.fa-check.fa-xs
    label.name(v-else for="select") {{ name }}

    div.buttns(v-if='!editing')
      i.fa.fa-pencil.fa-xs.action.edit(@click.stop="startEdition")
      i.fa.fa-trash-o.fa-xs.action.delete(v-on:click.stop="remove")
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
      return this.$store.state.singleton.inlineEditionId === this.wid;
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
        key: 'inlineEditionId',
        value: this.wid,
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
      this.$store.dispatch('singleton/unset', { key: 'inlineEditionId' });
    },
    remove() {
      this.$store.dispatch('wishGroup/removeWish', { wid: this.wid });
    },
  },
};
</script>

<style scoped>
.line:hover{
  cursor: pointer;
}
.line .name{
  cursor: pointer;
}
.line .buttns {
  visibility: hidden;
  position: absolute;
  top: 2px;
  right: 25px;
}
</style>

