<template lang='pug'>
  div.group.list-group-item.col-2(v-bind:class="{'bg-primary': isActive, 'bg-info': selected && ! isActive}" @click="toggleActivation")
    input(type="checkbox" name="selected" v-model="selected" @click="select" onclick="event.stopPropagation()")

    button.btn.btn-success.btn-sm(@click.stop="validEdition" onclick="event.stopPropagation()")(v-if='editing')
      i.fa.fa-check.fa-xs
    input(ref="editinput"
      v-model="editingName"
      v-on:keyup.enter="validEdition"
      v-on:blur="finishEdition")(v-if='editing')
    label.groupName(v-else for="selected") <strong>{{ name }}</strong>

    span ({{ selectedWishesNb }} / {{ wishesNb }})
    div.buttns-action(v-if='!editing')
      i.fa.fa-pencil.fa-xs.buttn-action(@click.stop="edit")
      i.fa.fa-trash-o.fa-xs.buttn-action(v-on:click.stop="remove")
</template>

<script>
import Vue from 'vue';

export default {
  props: ['gid'],
  data() {
    return {
      editingName: null,
    };
  },
  computed: {
    name() {
      return this.$store.getters.getWishGroup(this.gid).name;
    },
    isActive() {
      try {
        return this.gid === this.$store.getters.getActiveWishgid;
      } catch (e) {
        return false;
      }
    },
    editing() {
      return this.$store.getters.isEditing(this.gid);
    },
    selected() {
      return this.$store.getters.isSelectedWishGroup(this.gid);
    },
    wishesNb() {
      const predicate = e => (e.id === this.gid);
      return this.$store.state.wishGroups.filter(predicate)[0].wishes.length;
    },
    selectedWishesNb() {
      try {
        const selWishes = this.$store.state.currentBasket.selectedWishes;
        return Object.keys(selWishes[this.gid]).length;
      } catch (e) {
        return 0;
      }
    },
  },
  methods: {
    select() {
      this.$store.dispatch('selectWishGroup', {
        gid: this.gid,
        selected: !this.selected,
      });
    },
    toggleActivation() {
      this.$store.dispatch('toggleWishGroupActivation', this.gid);
    },
    focus() {
      this.$refs.editinput.focus();
    },
    edit() {
      this.editingName = this.name;
      this.$store.dispatch('setInlineEdition', this.gid);
      Vue.nextTick(this.focus);
    },
    finishEdition() {
      this.editingName = null;
      this.$store.dispatch('setInlineEdition', null);
    },
    validEdition() {
      this.$store.dispatch('renameWishGroup', {
        gid: this.gid,
        name: this.editingName,
      });
      this.name = this.editingName;
      this.finishEdition();
    },
    remove() {
      this.$store.dispatch('removeWishGroup', this.gid);
    },
  },
};
</script>

<style scoped>
.group{
  min-width: 150px;
  min-height: 50px;
}
.groupName{
  font-family: gunny;
  font-size: 2em;
  font-weight: bold;
}
.group .buttns-action {
  visibility: hidden;
  position: absolute;
  top: 2px;
  right: 2px;
}
.group:hover .buttns-action {
  visibility: visible;
}
.buttn-action {
  padding: 2px;
}
</style>
