<template lang="pug">
  div.line(v-bind:class="{'active': isActive, 'strong': selectedWishNb}"
      @click="setActivation")
    input(type="checkbox"
      name="selected"
      v-model="selected",
      :disabled="wishesNb === 0")
    input.edition(v-if='editing',
      ref="editinput",
      v-model="editingName",
      @click.stop="",
      @keyup.enter="validEdition",
      @keyup.esc="finishEdition")
    button.btn.btn-success.btn-sm.btn-edition(v-if='editing' @click="validEdition")
      i.fa.fa-check.fa-xs
    label.name(v-else for="selected") {{ name }}
    div.fakeCheckbox(v-if='!editing && wishesNb' @click="toggleSelection")
    div.confirmDeletion(v-if='deleting' @click="remove")
      span.btn.btn-danger Confirmer la suppression

    div.filling
      span {{ selectedWishNb }} / {{ wishesNb }}
    div.buttns(v-if='!editing')
      div.action.edit(@click.stop="startEdition")
        span.content renommer&nbsp;
        span.icon.fa.fa-pencil.fa-xs
      div.action.delete(@click.stop="startDeletion")
        span.icon.fa.fa-eraser.fa-xs
        span.content &nbsp;supprimer
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
      const action = this.$store.state.singleton.action;
      const gid = action.value ? action.value.gid : null;
      const type = action.type;
      return type === 'editGroup' && gid === this.gid;
    },
    deleting() {
      const action = this.$store.state.singleton.action;
      const gid = action.value && action.value.gid;
      const type = action.type;
      return type === 'deleteGroup' && gid === this.gid;
    },
    selected() {
      return this.$store.getters['selection/getSelectedGroupsIds'].indexOf(this.gid) !== -1;
    },
    selectedWishNb() {
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
      this.$store.dispatch('singleton/set', { activeGroupId: this.gid });
    },
    focus() {
      this.$refs.editinput.focus();
    },
    startEdition() {
      this.editingName = this.name;
      this.$store.dispatch('singleton/set', {
        action: {
          type: 'editGroup',
          value: {
            gid: this.gid,
          },
        },
      });
      Vue.nextTick(this.focus);
      this.setActivation();
    },
    finishEdition() {
      this.editingName = null;
      this.$store.dispatch('singleton/unset', 'action');
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
        action: {
          type: 'deleteGroup',
          value: {
            gid: this.gid,
          },
        },
      });
      this.setActivation();
    },
    finishDeletion() {
      this.$store.dispatch('singleton/unset', 'action');
    },
    remove() {
      const gids = this.$store.state.wishGroup.map(group => group.id);
      const currentPosition = gids.indexOf(this.gid);
      this.$store.dispatch('wishGroup/removeGroup', { gid: this.gid });
      this.$store.dispatch('singleton/set', { activeGroupId: gids[currentPosition - 1] });
    },
  },
};
</script>

<style scoped>
.line.active{
  background-color: var(--color3);
}
.filling {
  display: block;
  position: absolute;
  bottom: 0px;
  right: 0px;
}
.strong{
  font-weight: bold;
}
.content{
  visibility: hidden;
}
.wrapper{
  height: 30px;
  vertical-align: middle;
  line-height: 20px;
  padding: 5px;
}
</style>
