<template lang="pug">
  .wish.line(@click="select",
  :data-wid="wid",
  v-bind:class="{'active' : isCurrentWish}")
    span.product-name(v-bind:class="{'filled' : productIds.length > 0}") {{ wish.name }}
</template>

<script>

export default {
  props: ['wid', 'gid'],
  data() {
    return {
      editingId: 'summary-' + this.wid,
      editingName: null,
    };
  },
  computed: {
    isCurrentWish() {
      try {
        return this.wid === this.$store.getters['sectionWishes/getCurrent'].id;
      } catch (e) {
        return false;
      }
    },
    wish() {
      return this.$store.getters['wishGroup/getWish']({ wid: this.wid });
    },
    productIds() {
      const products = this.$store.state.selection.basket[this.wish.gid][this.wish.id];
      return products.map(p => p.pid).reverse();
    },
  },
  methods: {
    select() {
      this.$store.dispatch('sectionWishes/set', this.wid);
    },
  },
};
</script>

<style scoped>
.wish{
  padding-left: 10px;
  cursor: pointer;
  border-bottom: 1px dotted #72c4ff;
  overflow: hidden;
}
.wish:not(.active):hover{
  text-decoration: underline;
  background-color: #f0f0f0;
}
.filled{
  text-decoration: line-through;
/*  text-decoration: underline;
  text-decoration: overline;*/
}
.active{
  /*color: green;*/
  background-color: var(--active);
  /*text-decoration: underline;*/
}
</style>
