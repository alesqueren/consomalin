<template lang="pug">
  div.wish
    span.product-name {{ wish.name }}
</template>

<script>
import router from '../../router';

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
      return this.$store.getters['wishGroup/getWish']({ wid: this.wid });
    },
    productId() {
      return this.$store.state.selection[this.wish.gid][this.wish.id].pid;
    },
    productQuantity() {
      return this.$store.state.selection[this.wish.gid][this.wish.id].quantity;
    },
    productInfos() {
      return this.$store.state.product.details[this.productId];
    },
    total() {
      const total = this.productInfos.price * this.productQuantity;
      return parseFloat(total).toFixed(2);
    },
  },
  methods: {
    select() {
      this.$store.dispatch('sectionWishes/set', {
        wid: this.wid,
      }).then(
        this.$store.dispatch('product/fetchSearch', {
          name: this.wish.name,
        }),
        router.push({ name: 'section' }),
      );
    },
    increase() {
      if (this.productQuantity < 64) {
        const wid = this.wish.id;
        const pid = this.productId;
        const quantity = parseInt(this.productQuantity + 1, 10);
        this.$store.dispatch('selection/setWishProducts', { wid, pid, quantity });
      }
    },
    decrease() {
      if (this.productQuantity > 1) {
        const wid = this.wish.id;
        const pid = this.productId;
        const quantity = parseInt(this.productQuantity - 1, 10);
        this.$store.dispatch('selection/setWishProducts', { wid, pid, quantity });
      }
    },
    focus() {
      this.$refs.editinput.focus();
    },
    erase() {
      const wid = this.wid;
      const selected = false;
      this.$store.dispatch('selection/selectWish', { wid, selected });
    },
  },
};
</script>

<style scoped>
</style>
