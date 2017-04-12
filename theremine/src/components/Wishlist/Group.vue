<template lang='pug'>
  div.line(v-bind:class="{'active': isActive, 'strong': selectedWishesNb}"
      @click="setActivation")
    input(type="checkbox"
      name="selected"
      v-model="selected",
      :disabled="wishesNb === 0")
    input.edition(v-if='editing',
      ref="editinput"
      v-model="editingName"
      v-on:keyup.enter="validEdition"
      v-on:blur="finishEdition"
      v-on:keyup.esc="finishEdition")
    button.btn.btn-success.btn-sm.btn-edition(v-if='editing' @click.stop="validEdition" onclick="event.stopPropagation()" @keyup.esc="finishEdition")
      i.fa.fa-check.fa-xs
    label.name(v-else for="selected") {{ name }}
    div.fakeCheckbox(v-if='!editing && wishesNb' @click="toggleSelection")
    div.confirmDeletion(v-if='deleting' @click.stop="remove" @keyup.esc="finishDeletion")
      span.btn.btn-danger Confirmer la suppression"

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
      const actionnedEntity = this.$store.state.singleton.actionnedEntity;
      const actionnedEntityId = actionnedEntity.id;
      const action = actionnedEntity.action;
      return action === 'edit' && actionnedEntityId === this.gid;
    },
    deleting() {
      const actionnedEntity = this.$store.state.singleton.actionnedEntity;
      const actionnedEntityId = actionnedEntity.id;
      const action = actionnedEntity.action;
      return action === 'delete' && actionnedEntityId === this.gid;
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
        key: 'actionnedEntity',
        value: {
          action: 'edit',
          id: this.gid,
        },
      });
      Vue.nextTick(this.focus);
      this.setActivation();
    },
    finishEdition() {
      this.editingName = null;
      this.$store.dispatch('singleton/unset', { key: 'actionnedEntity' });
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
        key: 'actionnedEntity',
        value: {
          action: 'delete',
          id: this.gid,
        },
      });
      this.setActivation();
    },
    finishDeletion() {
      this.$store.dispatch('singleton/unset', {
        key: 'actionnedEntity',
      });
    },
    remove() {
      const gids = this.$store.state.wishGroup.map(group => group.id);
      const currentPosition = gids.indexOf(this.gid);

      this.$store.dispatch('wishGroup/removeGroup', { gid: this.gid });

      this.$store.dispatch('singleton/set', {
        key: 'activeGroupId',
        value: gids[currentPosition - 1],
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
</style>
