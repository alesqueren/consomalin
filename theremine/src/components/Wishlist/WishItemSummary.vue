<template lang='pug'>
  div.wish.list-group-item.col-xs-6
    div(v-if='isEditing')
      input(ref="editinput"
        v-model="editingName"
        v-on:keyup.enter="validEdition"
        v-on:blur="finishEdition")
      button.btn.btn-success.btn-sm(@click.stop="validEdition")
        i.fa.fa-check.fa-xs
    div(v-else)
      span {{ name }}

    div.buttons(v-if='!isEditing')
      i.fa.fa-edit.fa-xs(@click.stop="edit")
      i.fa.fa-eraser.fa-xs(@click.stop="unselect")
      i.fa.fa-trash-o.fa-xs(v-on:click.stop="remove")
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
    isEditing() {
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
      this.$store.dispatch('renameWish', {
        gid: this.gid,
        wid: this.wid,
        name: this.editingName,
      });
      this.finishEdition();
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

.wish {
  min-width: 250px;
}

.wish i {
  visibility: hidden;
}

.wish:hover i {
  visibility: visible;
}

.buttons {
  position: absolute;
  top: 5px;
  right: 5px;
}

</style>
