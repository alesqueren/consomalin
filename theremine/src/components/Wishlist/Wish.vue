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

    div.buttns(v-if='!editing')
      div.action.edit(@click.stop="startEdition")
        span.content Renommer&nbsp;
        span.icon.fa.fa-pencil
      div.action.delete(@click.stop="erase", v-bind:class="{'deleting': deleting}")
        span.icon.fa.fa-eraser
        span &nbsp;
        span.content {{deleteWording}}
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
        return Boolean(this.$store.state.selection.basket[this.gid][this.wid]);
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
    deleteWording() {
      return this.deleting ? 'valider ?' : 'effacer';
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
    erase() {
      if (!this.deleting) {
        this.startDeletion();
      } else {
        this.remove();
      }
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
.deleting{
  visibility: visible;
}
</style>

