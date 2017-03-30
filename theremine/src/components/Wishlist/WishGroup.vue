<template lang='pug'>
  div.wishgroup.list-group-item.col-2(v-bind:class="{'bg-info': selected}")
    input(type="checkbox" v-model="selected" @click="select")

    div(v-if='editing')
      input(ref="editinput" 
        v-model="editingName" 
        v-on:keyup.enter="validEdition" 
        v-on:blur="cancelEdition")
      button.btn.btn-success.btn-sm(@click.stop="validEdition")
        i.fa.fa-check.fa-xs
    div(v-else)
      span.groupName <strong>{{ name }}</strong>

    span ({{ selectedWishesNb }} / {{ wishesNb }})
    div(v-if='!editing')
      button.dynamic.btn.btn-primary.btn-sm(@click.stop="edit")
        i.fa.fa-pencil.fa-xs
      button.dynamic.btn.btn-danger.btn-sm(@click.stop="remove")
        i.fa.fa-trash-o.fa-xs
</template>

<script>
import Vue from 'vue';

export default {
  props: ['wishgroup'],
  data() {
    return {
      name: this.wishgroup.name,
      editingName: null,
    };
  },
  computed: {
    editing() {
      return Boolean(this.$store.state.inlineEdition === this.wishgroup.id);
    },
    selected() {
      return this.$store.getters.isSelectedWishGroup(this.wishgroup.id);
    },
    wishesNb() {
      const predicate = e => (e.id === this.wishgroup.id);
      return this.$store.state.wishGroups.filter(predicate)[0].wishes.length;
    },
    selectedWishesNb() {
      try {
        const selWishes = this.$store.state.currentBasket.selectedWishes;
        return Object.keys(selWishes[this.wishgroup.id]).length;
      } catch (e) {
        return 0;
      }
    },
  },
  methods: {
    select() {
      this.$store.dispatch('selectWishGroup', {
        gid: this.wishgroup.id,
        selected: !this.selected,
      });
    },
    focus() {
      this.$refs.editinput.focus();
    },
    edit() {
      this.editingName = this.name;
      this.$store.dispatch('setInlineEdition', this.wishgroup.id);
      // document.getElementById(this.wishgroup.id).focus();
      Vue.nextTick(this.focus);
    },
    cancelEdition() {
      console.log('cancel');
      this.editingName = null;
      this.$store.dispatch('setInlineEdition', null);
    },
    validEdition() {
      this.name = this.editingName;
      this.$store.dispatch('renameWishGroup', {
        gid: this.wishgroup.id,
        name: this.name,
      });
    },
    remove() {
      this.$store.dispatch('removeWishGroup', this.wishgroup.id);
    },
  },
};
</script>


<style scoped>
.wishgroup button.dynamic {
  visibility: hidden;
}
.wishgroup:hover button.dynamic {
  visibility: visible;
}
</style>
