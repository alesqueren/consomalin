<template lang="pug">
  .wish(@click="select")
    i.fa.fa-arrow-right.active(v-if="isCurrentWish") &nbsp;
    span.product-name(v-bind:class="{'filled' : productIds.length > 0, 'active' : isCurrentWish}") {{ wish.name }}
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
      return this.wid === this.$store.getters['sectionWishes/getCurrent'].id;
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
  margin-left: 50px;
  cursor: pointer;
}
.filled{
  text-decoration: line-through;
}
.active{
  color: green;
  font-weight: bold;
}
</style>
