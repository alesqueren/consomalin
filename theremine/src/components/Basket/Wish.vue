<template lang='pug'>
  div.wish.list-group-item
    span.fa.fa-eraser.wish-remove(@click.prevent.stop='removeWish($event)')
    div
      span.wishgroupname.badge.badge-success {{wish.gname}}
    div(v-if='productInfos')
      div  {{productInfos.name}}
      |  
      img.col-md-6(style='width:50px;', v-bind:src='productInfos.imageUrl')
      |  
      div
        input(type='number', v-model.number='quantity', step='1', value='0', min='1', max='256' @click.prevent.stop='')
        span &nbsp;&nbsp;&nbsp;&nbsp;{{total}}€
    div(v-if='isEditing')
      input(ref="editinput"
        v-model="editingName"
        v-on:keyup.enter="validEdition"
        v-on:blur="finishEdition")
      button.btn.btn-success.btn-sm(@click.stop="validEdition")
        i.fa.fa-check.fa-xs
    div(v-else)
      span {{ wish.name }}
    div.buttons(v-if='!isEditing')
      i.fa.fa-edit.fa-xs(@click.stop="edit")
      i.fa.fa-eraser.fa-xs(@click.stop="unselect")
      i.fa.fa-trash-o.fa-xs(v-on:click.stop="remove")
</template>

<script>
import Vue from 'vue';

export default {
  props: ['wid', 'gid'],
  data() {
    return {
      editingId: 'summary-' + this.wid,
      editingName: null,
    };
  },
  computed: {
    quantity: {
      get() {
        return this.productQuantity;
      },
      set(quantity) {
        const gid = this.wish.gid;
        const wid = this.wish.id;
        const pid = this.productId;
        this.$store.dispatch('updateWishProduct', { gid, wid, pid, quantity });
      },
    },
    wish() {
      const wish = this.$store.getters.getWish(this.wid);
      return this.$store.getters.getWish(this.wid);
    },
    productId() {
      return this.$store.state.currentBasket.selectedWishes[this.wish.gid][this.wish.id].pid;
    },
    productQuantity() {
      return this.$store.state.currentBasket.selectedWishes[this.wish.gid][this.wish.id].quantity;
    },
    productInfos() {
      return this.$store.state.productInfos[this.productId];
    },
    isEditing() {
      return this.$store.getters.isEditing(this.editingId);
    },
    total() {
      const total = this.productInfos.price * this.productQuantity;
      return parseInt(total, 10);
    },
  },
  methods: {
    unselect() {
      this.$store.dispatch('selectWish', {
        gid: this.gid,
        wid: this.wid,
        selected: false,
      });
    },
    focus() {
      this.$refs.editinput.focus();
    },
    edit() {
      this.editingName = this.name;
      this.$store.dispatch('setInlineEdition', this.editingId);
      Vue.nextTick(this.focus);
    },
    finishEdition() {
      this.editingName = null;
      this.$store.dispatch('setInlineEdition', null);
    },
    validEdition() {
      this.$store.dispatch('renameWish', {
        gid: this.gid,
        wid: this.wid,
        name: this.editingName,
      });
      this.finishEdition();
    },
    remove() {
      this.$store.dispatch('removeWish', {
        gid: this.gid,
        wid: this.wid,
      });
    },
  },
};
</script>

<style scoped>

.wish {
  min-width: 200px;
}

.wish i {
  visibility: hidden;
}

.wish:hover i {
  visibility: visible;
}

.buttons {
  position: absolute;
  top: 5px;
  right: 5px;
}

</style>
