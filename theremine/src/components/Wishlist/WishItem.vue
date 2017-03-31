<template lang='pug'>
  div.wish.list-group-item.col-xs-6(v-on:click.stop="select")
    input(type="checkbox" v-model="selected" onclick="event.cancelBubble=true;")
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
      button.btn.btn-danger.btn-sm(v-on:click.stop="remove")
        i.fa.fa-trash-o.fa-xs
</template>

<script>
import Vue from 'vue';

export default {
  props: ['wish', 'gid'],
  data() {
    return {
      name: this.wish.name,
      editingName: null,
    };
  },
  computed: {
    selected: {
      get() {
        const wishIsSelected = this.$store.getters.isSelectedWish(
          {
            gid: this.gid,
            wid: this.wish.id,
          },
        );
        return wishIsSelected;
      },
      set(selected) {
        this.$store.dispatch('selectWish', {
          wid: this.wish.id,
          selected,
        });
      },
    },
    editing() {
      return this.$store.getters.isEditing(this.wish.id);
    },
  },
  methods: {
    select() {
      this.$store.dispatch('selectWish', {
        wid: this.wish.id,
        selected: !this.selected,
      });
    },
    focus() {
      this.$refs.editinput.focus();
    },
    edit() {
      this.editingName = this.name;
      this.$store.dispatch('setInlineEdition', this.wish.id);
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
        wid: this.wish.id,
        name: this.name,
      });
    },
    remove() {
      this.$store.dispatch('removeWish', {
        gid: this.gid,
        wid: this.wish.id,
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
