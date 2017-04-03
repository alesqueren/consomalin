<template lang='pug'>
transition(name="fade")
  .wish.list-group-item(v-bind:class='{active:wishIsCurrent}', @click='setCurrentWish()')
    span.fa.fa-eraser.wish-remove(@click.prevent.stop='removeWish($event)')
    div
      span.wishgroupname.badge.badge-success {{wish.gname}}
      span.wishname {{wish.name}}
    div(v-if='wish.product.infos')
      div  {{wish.product.infos.name}}
      |  
      img.col-md-6(style='width:50px;', v-bind:src='wish.product.infos.imageUrl')
      |  
      div
        input(type='number', v-model.number='quantity', step='1', value='0', min='1', max='256' @click.prevent.stop='log')
        span &nbsp;&nbsp;&nbsp;&nbsp;{{total}}€

</template>

<script>
export default {
  props: ['wish'],
  computed: {
    quantity: {
      get() {
        return this.wish.product.quantity;
      },
      set(quantity) {
        const gid = this.wish.gid;
        const wid = this.wish.id;
        const pid = this.wish.product.id;
        this.$store.dispatch('updateWishProduct', { gid, wid, pid, quantity });
      },
    },
    wishIsCurrent() {
      const currentWishId = this.$store.state.currentBasket.currentWishId;
      return this.wish.id === currentWishId;
    },
    total() {
      const total = this.wish.product.infos.price * this.wish.product.quantity;
      return parseInt(total, 10);
    },
  },
  methods: {
    removeWish() {
      const wid = this.wish.id;
      const selected = false;
      this.$store.dispatch('selectWish', { wid, selected }).then(() => {
        if (this.wishIsCurrent) {
          this.$store.dispatch('nextCurrentWish');
        }
      });
    },
    setCurrentWish() {
      const gid = this.wish.gid;
      const wid = this.wish.id;
      const name = this.wish.name;
      this.$store.dispatch('setCurrentWish', { gid, wid });
      this.$store.dispatch('searchProductsWithName', { name });
    },
    changeQty() {
      // console.log('log2');
      const gid = this.wish.gid;
      const wid = this.wish.id;
      const pid = this.wish.product.id;
      const quantity = this.wish.product.quantity;
      this.$store.dispatch('updateWishProduct', { gid, wid, pid, quantity });
    },
    log() {
      // console.log('log');
    },
  },
};
</script>

<style scoped>
.wish{
  padding: 5px;
  min-height: 85px;
  position: relative;
}
.wishgroupname{
  position: absolute;
  top: 5px;
  left: 5px;
}
.wishname{
  font-weight: bold;
}
.wish-remove{
  position: absolute;
  top: 5px;
  right: 5px;
  color: red;
  z-index: 10;
}
.fade-enter-active, .fade-leave-active {
  transition: opacity .5s
}
.fade-enter, .fade-leave-to {
  opacity: 0
}
</style>
