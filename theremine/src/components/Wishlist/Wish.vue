<template lang="pug">
  div.line(v-on:click.stop="select")
    div.confirmUncheck(v-if="unchecking")
      span.fa.fa-warning &nbsp;
    input(type="checkbox" name="select" v-model="selected")
    input.edition(v-if='editing'
      ref="editinput",
      v-model="editingName",
      @click.stop="",
      v-on:keyup.enter="validEdition",
      v-on:keyup.esc="finishEdition")
    button.btn.btn-success.btn-sm.btn-edition(v-if='editing' @click="validEdition")
      i.fa.fa-check.fa-xs
    label.name(v-else for="select")
      .nameIn {{ name }}
    div.fakeCheckbox(v-if='!editing' @click.stop="select")
    .added.fa.fa-cart-arrow-down.tooltip(v-if="productsNb")
      span.tooltiptext.tooltip-bottom Vous avez déjà ajouté ce produit au panier
    div.buttns(v-if='!editing')
      div.action.edit(@click.stop="startEdition")
        span.content Renommer&nbsp;
        span.icon.fa.fa-pencil
      div.action.delete(@click.stop="erase", v-bind:class="{'deleting': deleting}")
        span.icon.fa.fa-eraser
        span &nbsp;
        span.content {{deleteWording}}
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
        return Boolean(this.$store.state.selection.basket[this.gid][this.wid]);
      } catch (e) {
        return false;
      }
    },
    action() {
      return this.$store.state.singleton.action;
    },
    actionWid() {
      return this.action.value.wid;
    },
    actionType() {
      return this.action.type;
    },
    editing() {
      return this.actionType === 'editWish' && this.actionWid === this.wid;
    },
    deleting() {
      return this.actionType === 'deleteWish' && this.actionWid === this.wid;
    },
    unchecking() {
      return this.actionType === 'uncheckWish' && this.actionWid === this.wid;
    },
    deleteWording() {
      return this.deleting ? 'Valider ?' : 'Effacer';
    },
    productsNb() {
      try {
        const products = this.$store.state.selection.basket[this.gid][this.wid];
        return products.map(p => p.pid).length;
      } catch (e) {
        return 0;
      }
    },
  },
  methods: {
    select() {
      if (this.selected && this.productsNb && !this.unchecking) {
        this.startUncheck();
      } else {
        this.$store.dispatch('singleton/unset', 'action');
        this.$store.dispatch('selection/selectWish', {
          wid: this.wid,
          selected: !this.selected,
        });
      }
    },
    focus() {
      this.$refs.editinput.focus();
    },
    erase() {
      if (!this.deleting) {
        this.startDeletion();
      } else {
        this.remove();
      }
    },
    startEdition() {
      this.editingName = this.name;
      this.$store.dispatch('singleton/set', {
        action: {
          type: 'editWish',
          value: {
            wid: this.wid,
          },
        },
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
      this.$store.dispatch('singleton/unset', 'action');
    },
    startDeletion() {
      this.$store.dispatch('singleton/set', {
        action: {
          type: 'deleteWish',
          value: {
            wid: this.wid,
          },
        },
      });
    },
    finishDeletion() {
      this.$store.dispatch('singleton/unset', 'action');
    },
    remove() {
      this.$store.dispatch('wishGroup/removeWish', { wid: this.wid });
    },
    startUncheck() {
      this.$store.dispatch('singleton/set', {
        action: {
          type: 'uncheckWish',
          value: {
            wid: this.wid,
          },
        },
      });
    },
  },
};
</script>

<style scoped>
.deleting{
  visibility: visible;
}
.nameIn{
  overflow: hidden;
  height: 50px;
}
.confirmUncheck{
  position: absolute;
  background-color: #f0ad4e;
  height: 49px;
  width: 65px;
  bottom: 0px;
  left: 0px;
  /*z-index: 1;*/
  font-family: helvetica;
  font-size: 13px;
  color: white;
  padding: 2px;
}
.confirmUncheck .fa-warning{
  position: absolute;
  bottom: 5px;
  right: 4px;
  font-size: 13px;
  width: 12px;
  height: 12px;
  color: black;
}
.added {
  position: absolute;
  bottom: 14px;
  right: 15px;
  font-size: 20px;
}
</style>

