<template lang="pug">
  div.line(v-on:click="select")
    input(type="checkbox" name="select" v-model="selected")
    input.edition(v-if='editing'
      ref="editinput",
      v-model="editingName",
      @click.stop="",
      v-on:keyup.enter="validEdition",
      v-on:keyup.esc="finishEdition")
    button.btn.btn-success.btn-sm.btn-edition(v-if='editing' @click="validEdition")
      i.fa.fa-check.fa-xs
    label.name(v-else for="select") {{ name }}
    div.confirmDeletion(v-if='deleting' @click="remove")
      span.btn.btn-danger Confirmer la suppression

    div.buttns(v-if='!editing')
      div.action.edit(@click.stop="startEdition")
        span.content renommer&nbsp;
        span.icon.fa.fa-pencil
      div.action.delete(@click.stop="startDeletion")
        span.icon.fa.fa-eraser
        span.content &nbsp;&nbsp;&nbsp;&nbsp;effacer
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
      const action = this.$store.state.singleton.action;
      const wid = action.value && action.value.wid;
      const type = action.type;
      return type === 'editWish' && wid === this.wid;
    },
    deleting() {
      const action = this.$store.state.singleton.action;
      const wid = action.value && action.value.wid;
      const type = action.type;
      return type === 'deleteWish' && wid === this.wid;
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
        action: {
          type: 'editWish',
          value: {
            wid: this.wid,
          },
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
      this.$store.dispatch('singleton/unset', 'action');
    },
    startDeletion() {
      this.$store.dispatch('singleton/set', {
        action: {
          type: 'deleteWish',
          value: {
            wid: this.wid,
          },
        },
      });
    },
    finishDeletion() {
      this.$store.dispatch('singleton/unset', 'action');
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
