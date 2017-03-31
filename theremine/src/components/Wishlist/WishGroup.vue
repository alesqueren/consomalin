<template lang='pug'>
  div.wishgroup.list-group-item.col-2(v-bind:class="{'bg-primary': isActive, 'bg-info': selected && ! isActive}" @click="toggleActivation")
    input(type="checkbox" v-model="selected" @click="select" onclick="event.stopPropagation()")

    div(v-if='editing')
      button.btn.btn-success.btn-sm(@click.stop="validEdition" onclick="event.stopPropagation()")
        i.fa.fa-check.fa-xs
      input(ref="editinput"
        v-model="editingName"
        v-on:keyup.enter="validEdition"
        v-on:blur="finishEdition")
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
      editingName: null,
    };
  },
  computed: {
    // todo: remove?
    name() {
      return this.$store.getters.getWishGroup(this.wishgroup.id).name;
    },
    isActive() {
      try {
        return this.wishgroup.id === this.$store.getters.getActiveWishGroup.id;
      } catch (e) {
        return false;
      }
    },
    editing() {
      return this.$store.getters.isEditing(this.wishgroup.id);
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
    toggleActivation() {
      this.$store.dispatch('toggleWishGroupActivation', this.wishgroup.id);
    },
    focus() {
      this.$refs.editinput.focus();
    },
    edit() {
      this.editingName = this.name;
      this.$store.dispatch('setInlineEdition', this.wishgroup.id);
      Vue.nextTick(this.focus);
    },
    finishEdition() {
      console.log('finish');
      this.editingName = null;
      this.$store.dispatch('setInlineEdition', null);
    },
    validEdition() {
      console.log('valid0');
      this.$store.dispatch('renameWishGroup', {
        gid: this.wishgroup.id,
        name: this.editingName,
      });
      console.log('valid1');
      this.name = this.editingName;
      console.log('valid2');
      this.finishEdition();
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
