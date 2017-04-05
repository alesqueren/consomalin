<template lang='pug'>
  div.wish.list-group-item.col-xs-6(v-on:click.stop="select")
    input(type="checkbox" name="select" v-model="selected" onclick="event.cancelBubble=true;")
    input(ref="editinput" 
      v-model="editingName"
      v-on:keyup.enter="validEdition"
      v-on:blur="finishEdition")(v-if='editing')
    button.btn.btn-success.btn-sm(@click.stop="validEdition" v-if='editing')
      i.fa.fa-check.fa-xs
    label.wish-name(v-else for="select") {{ name }}

    div.buttns-action(v-if='!editing')
      i.fa.fa-pencil.fa-xs.buttn-action(@click.stop="edit")
      i.fa.fa-trash-o.fa-xs.buttn-action(v-on:click.stop="remove")
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
        wid: this.wish.id,
      });
    },
  },
};
</script>

<style scoped>
.wish-name{
  font-family: gunny;
  font-size: 1.5em;
  font-weight: bold;
}
.wish .buttns-action {
  visibility: hidden;
  position: absolute;
  top: 2px;
  right: 2px;
}
.wish:hover .buttns-action {
  visibility: visible;
}
.buttn-action {
  padding: 2px;
}
/* Cachons la case à cocher */
[type="checkbox"]:not(:checked),
[type="checkbox"]:checked {
  position: absolute;
  left: -9999px;
}

[type="checkbox"]:not(:checked) + label,
[type="checkbox"]:checked + label {
  position: relative;
  padding-left: 35px;
  cursor: pointer;
}
[type="checkbox"]:not(:checked) + label:before,
[type="checkbox"]:checked + label:before {
  content: '';
  position: absolute;
  left:0; top: 2px;
  width: 25px; height: 25px;
  border: 1px solid #aaa;
  background: #f8f8f8;
  border-radius: 3px;
  box-shadow: inset 0 1px 3px rgba(0,0,0,.3)
}

[type="checkbox"]:not(:checked) + label:after,
[type="checkbox"]:checked + label:after {
  content: '✔';
  position: absolute;
  top: -2px; left: 1px;
  font-size: 30px;
  color: #09ad7e;
  transition: all .2s;
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
