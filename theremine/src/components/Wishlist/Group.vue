<template lang='pug'>
  div.group(v-bind:class="{'active': isActive, 'strong': selectedWishesNb}"
      @click="setActivation")
    input(type="checkbox"
      name="selected"
      v-model="selected",
      :disabled="wishesNb === 0")
    input(v-if='editing',
      ref="editinput"
      v-model="editingName"
      v-on:keyup.enter="validEdition"
      v-on:blur="finishEdition")
    button.btn.btn-success.btn-sm(v-if='editing' @click.stop="validEdition" onclick="event.stopPropagation()" @keyup.esc="finishEdition")
      i.fa.fa-check.fa-xs
    label.groupName(v-else for="selected") {{ name }}
    div.groupCheckbox(v-if='!editing && wishesNb' @click="toggleSelection")

    div.filling
      span {{ selectedWishesNb }} / {{ wishesNb }}
    div.buttns-action(v-if='!editing')
      i.fa.fa-pencil.fa-xs.buttn-action(@click.stop="startEdition")
      i.fa.fa-trash-o.fa-xs.buttn-action(v-on:click.stop="remove")
</template>

<script>
import Vue from 'vue';

export default {
  props: ['gid'],
  data() {
    return {
      editingName: null,
      deleting: false,
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
    remove() {
      this.$store.dispatch('wishGroup/removeGroup', { gid: this.gid });
    },
  },
};
</script>

<style scoped>
.group{
/*  min-width: 150px;
  min-height: 50px;*/
  cursor: pointer;
  position: relative;
  list-style: none;
  border-bottom: 1px dotted #72c4ff ;
  text-indent: 25px;
  height: 50px;
  line-height: 50px;
  padding: 0 10px 0 10px;
  text-transform: capitalize;
}
.active{
  background-color: #5bc0de!important;
}
.group:hover{
  background-color: #f0f0f0;
  -webkit-transition: all 0.2s;
  -moz-transition:    all 0.2s;
  -ms-transition:     all 0.2s;
  -o-transition:      all 0.2s;
}
.groupName{
  cursor: default;
  font-family: gunny;
  font-size: 2em;
}
.group .buttns-action {
  visibility: hidden;
  position: absolute;
  top: 2px;
  right: 75px;
}
.group:hover .buttns-action {
  visibility: visible;
  cursor: pointer;
}
.buttn-action {
  display: inline!important;
  padding: 8px;
  width: 30px;
}
.buttn-action:hover {
  font-size: 1.1em;
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
.groupCheckbox{
  cursor: pointer;
  position: absolute;
  top: 0;
  left: 0;
  width: 65px;
  height: 50px;

}
[type="checkbox"]:not(:checked),
[type="checkbox"]:checked {
  position: absolute;
  left: -9999px;
}

[type="checkbox"]:not(:checked) + label,
[type="checkbox"]:checked + label {
  position: relative;
  padding-left: 35px;
}
[type="checkbox"]:not(:checked) + label:before,
[type="checkbox"]:checked + label:before {
  content: '';
  position: absolute;
  left:-16px; top: 12px;
  width: 25px; height: 25px;
  border: 1px solid #aaa;
  background: #f8f8f8;
  border-radius: 3px;
  box-shadow: inset 0 1px 3px rgba(0,0,0,.3);
}
[type="checkbox"]:not(:checked):disabled + label:before,
[type="checkbox"]:checked:disabled + label:before {
  box-shadow: inset 0 1px 28px rgba(0,0,0,.3);
}

[type="checkbox"]:not(:checked):not(:disabled) + label:after,
[type="checkbox"]:checked:not(:disabled) + label:after {
  content: '✔';
  position: absolute;
  top: 2px; left: -40px;
  font-size: 30px;
  color: #09ad7e;
  transition: all .1s;
}

[type="checkbox"]:not(:checked) + label:after {
  opacity: 0;
  transform: scale(0);
}
[type="checkbox"]:checked + label:after {
  opacity: 1;
  transform: scale(1);
}
</style>
