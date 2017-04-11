<template lang='pug'>
  div.line(v-bind:class="{'active': isActive, 'strong': selectedWishesNb}"
      @click="setActivation")
    input(type="checkbox"
      name="selected"
      v-model="selected",
      :disabled="wishesNb === 0")
    input(v-if='editing',
      ref="editinput"
      v-model="editingName"
      v-on:keyup.enter="validEdition"
      v-on:keyup.esc="finishEdition"
      v-on:blur="finishEdition")
    button.btn.btn-success.btn-sm(v-if='editing' @click.stop="validEdition" onclick="event.stopPropagation()" @keyup.esc="finishEdition")
      i.fa.fa-check.fa-xs
    label.name(v-else for="selected") {{ name }}
    div.fakeCheckbox(v-if='!editing && wishesNb' @click="toggleSelection")
    div.confirmDeletion(v-if='deleting' @click.stop="remove" @keyup.esc="finishDeletion")
      span.btn.btn-danger Confirmer la suppression du groupe "{{ name }}"

    div.filling
      span {{ selectedWishesNb }} / {{ wishesNb }}
    div.buttns(v-if='!editing')
      i.fa.fa-pencil.fa-xs.action.edit(@click.stop="startEdition")
      i.fa.fa-trash-o.fa-xs.action.delete(@click.stop="startDeletion")
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
      return this.$store.getters['wishGroup/getGroup']({ gid: this.gid }).name;
    },
    isActive() {
      return this.gid === this.$store.state.singleton.activeGroupId;
    },
    editing() {
      return this.$store.state.singleton.inlineEditionId === this.gid;
    },
    deleting() {
      return this.$store.state.singleton.deletingGroup === this.gid;
    },
    selected() {
      return this.$store.getters['selection/getSelectedGroupsIds'].indexOf(this.gid) !== -1;
    },
    selectedWishesNb() {
      return this.$store.getters['selection/getSelectedWishesByGroup']({ gid: this.gid }).length;
    },
    wishesNb() {
      const predicate = e => (e.id === this.gid);
      return this.$store.state.wishGroup.filter(predicate)[0].wishes.length;
    },
  },
  methods: {
    toggleSelection() {
      const actionName = !this.selected ? 'selectGroup' : 'unselectGroup';
      this.$store.dispatch('selection/' + actionName, { gid: this.gid });
    },
    setActivation() {
      this.$store.dispatch('singleton/set', {
        key: 'activeGroupId',
        value: this.gid,
      });
    },
    focus() {
      this.$refs.editinput.focus();
    },
    startEdition() {
      this.editingName = this.name;
      this.$store.dispatch('singleton/set', {
        key: 'inlineEditionId',
        value: this.gid,
      });
      Vue.nextTick(this.focus);
    },
    finishEdition() {
      this.editingName = null;
      this.$store.dispatch('singleton/unset', { key: 'inlineEditionId' });
    },
    validEdition() {
      this.$store.dispatch('wishGroup/renameGroup', {
        gid: this.gid,
        name: this.editingName,
      });
      this.finishEdition();
    },
    startDeletion() {
      this.$store.dispatch('singleton/set', {
        key: 'deletingGroup',
        value: this.gid,
      });
    },
    finishDeletion() {
      this.$store.dispatch('singleton/unset', {
        key: 'deletingGroup',
      });
    },
    remove() {
      this.$store.dispatch('wishGroup/removeGroup', { gid: this.gid });

      this.$store.dispatch('singleton/set', {
        key: 'activeGroupId',
        value: this.$store.state.wishGroup.map(group => group.id)[0],
      });
    },
  },
};
</script>

<style scoped>
.line.active{
  background-color: #5bc0de!important;
}
.filling {
  display: block;
  position: absolute;
  bottom: -2px;
  right: 5px;
}
.strong{
  font-weight: bold;
}
.confirmDeletion{
  text-transform: none;
  cursor: pointer;
  position: absolute;
  font-size: 1em;
  font-family: arial;
  top: 0;
  right: 25px;
  z-index: 2;
}
</style>
