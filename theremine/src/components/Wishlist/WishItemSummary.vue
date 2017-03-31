<template lang='pug'>
  div.wish.list-group-item.col-xs-6
    div(v-if='editing')
      input(ref="editinput"
        v-model="editingName"
        v-on:keyup.enter="validEdition"
        v-on:blur="finishEdition")
      button.btn.btn-success.btn-sm(@click.stop="validEdition")
        i.fa.fa-check.fa-xs
    div(v-else)
      span {{ name }}

    div(v-if='!editing')
      button.btn.btn-primary.btn-sm(@click.stop="edit")
        i.fa.fa-pencil.fa-xs
      button.btn.btn-warning.btn-sm(@click.stop="unselect")
        i.fa.fa-close.fa-xs
      button.btn.btn-danger.btn-sm(v-on:click.stop="remove")
        i.fa.fa-trash-o.fa-xs
</template>

<script>
import Vue from 'vue';

export default {
  props: ['wid', 'gid'],
  data() {
    return {
      editingId: 'summary-' + this.wid,
      editingName: null,
    };
  },
  computed: {
    name() {
      return this.$store.getters.getWish(this.wid).name;
    },
    editing() {
      return this.$store.getters.isEditing(this.editingId);
    },
  },
  methods: {
    unselect() {
      this.$store.dispatch('selectWish', {
        gid: this.gid,
        wid: this.wid,
        selected: false,
      });
    },
    focus() {
      this.$refs.editinput.focus();
    },
    edit() {
      this.editingName = this.name;
      this.$store.dispatch('setInlineEdition', this.editingId);
      Vue.nextTick(this.focus);
    },
    finishEdition() {
      this.editingName = null;
      this.$store.dispatch('setInlineEdition', null);
    },
    validEdition() {
      this.name = this.editingName;
      this.finishEdition();
      this.$store.dispatch('renameWish', {
        gid: this.gid,
        wid: this.wid,
        name: this.name,
      });
    },
    remove() {
      this.$store.dispatch('removeWish', {
        gid: this.gid,
        wid: this.wid,
      });
    },
  },
};
</script>

<style scoped>
.wish button {
  visibility: hidden;
}
.wish:hover button {
  visibility: visible;
}
</style>
