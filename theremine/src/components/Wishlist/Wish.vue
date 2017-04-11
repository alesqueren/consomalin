<template lang='pug'>
  div.wish(v-on:click.stop="select")
    input(type="checkbox" name="select" v-model="selected")
    input(v-if='editing'
      ref="editinput" 
      v-model="editingName"
      v-on:keyup.enter="validEdition"
      v-on:blur="finishEdition")
    button.btn.btn-success.btn-sm(v-if='editing' @click.stop="validEdition")
      i.fa.fa-check.fa-xs
    label.wish-name(v-else for="select") {{ name }}

    div.buttns-action(v-if='!editing')
      i.fa.fa-pencil.fa-xs.buttn-action(@click.stop="startEdition")
      i.fa.fa-trash-o.fa-xs.buttn-action(v-on:click.stop="remove")
</template>

<script>
import Vue from 'vue';

export default {
  props: ['gid', 'wid'],
  data() {
    return {
      editingName: null,
    };
  },
  computed: {
    name() {
      return this.$store.getters['wishGroup/getWish']({ wid: this.wid }).name;
    },
    selected() {
      try {
        return Boolean(this.$store.state.selection[this.gid][this.wid]);
      } catch (e) {
        return false;
      }
    },
    editing() {
      return this.$store.state.singleton.inlineEditionId === this.wid;
    },
  },
  methods: {
    select() {
      this.$store.dispatch('selection/selectWish', {
        wid: this.wid,
        selected: !this.selected,
      });
    },
    focus() {
      this.$refs.editinput.focus();
    },
    startEdition() {
      this.editingName = this.name;
      this.$store.dispatch('singleton/set', {
        key: 'inlineEditionId',
        value: this.wid,
      });
      Vue.nextTick(this.focus);
    },
    validEdition() {
      this.$store.dispatch('wishGroup/renameWish', {
        wid: this.wid,
        name: this.editingName,
      });
      this.finishEdition();
    },
    finishEdition() {
      this.editingName = null;
      this.$store.dispatch('singleton/unset', { key: 'inlineEditionId' });
    },
    remove() {
      this.$store.dispatch('wishGroup/removeWish', { wid: this.wid });
    },
  },
};
</script>

<style scoped>
.wish{
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
.wish-name{
  font-family: gunny;
  font-size: 2em;
}
.wish .buttns-action {
  visibility: hidden;
  position: absolute;
  top: 2px;
  right: 2px;
}
.wish:hover .buttns-action {
  visibility: visible;
  cursor: pointer;
}
.buttn-action {
  padding: 2px;
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

