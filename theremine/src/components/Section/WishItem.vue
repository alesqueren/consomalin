<template lang='pug'>
transition(name="fade")
  .wish.list-group-item(v-bind:class='{active:wishIsCurrent}', @click='setCurrentWish()')
    span.fa.fa-eraser.wish-erase(@click.prevent.stop='removeWish($event)')
    div
      span.wishgroupname.badge.badge-success {{wish.gname}}
      span.wishname {{wish.name}}
    div(v-if='wish.product.infos')
      div  {{wish.product.infos.name}}
      |  
      img(style='width:150px;', v-bind:src='wish.product.infos.imageUrl')
      |  
      div
        div.count-input.space-bottom
          a.incr-btn(@click.prevent.stop='decrease' href="#") –
          input.quantity(type='number', v-model.number='quantity', step='1', value='0', min='1', max='256' @click.prevent.stop='')
          a.incr-btn(@click.prevent.stop='increase' href="#") &plus;
        span.total &nbsp;&nbsp;&nbsp;&nbsp;{{total}}€

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
        this.$store.dispatch('wishlist/product/updateWishProduct', {
          gid: this.wish.gid,
          wid: this.wish.id,
          pid: this.wish.product.id,
          quantity,
        });
      },
    },
    wishIsCurrent() {
      const currentWishId = this.$store.state.basket.currentWishId;
      return this.wish.id === currentWishId;
    },
    total() {
      const total = this.wish.product.infos.price * this.wish.product.quantity;
      return parseInt(total, 10);
    },
  },
  methods: {
    increase() {
      if (this.productQuantity < 64) {
        const gid = this.wish.gid;
        const wid = this.wish.id;
        const pid = this.productId;
        const quantity = this.productQuantity + 1;
        this.$store.dispatch('updateWishProduct', { gid, wid, pid, quantity });
      }
    },
    decrease() {
      if (this.productQuantity > 1) {
        const gid = this.wish.gid;
        const wid = this.wish.id;
        const pid = this.productId;
        const quantity = this.productQuantity - 1;
        this.$store.dispatch('updateWishProduct', { gid, wid, pid, quantity });
      }
    },
    removeWish() {
      const wid = this.wish.id;
      const selected = false;
      this.$store.dispatch('basket/selectWish', { wid, selected }).then(() => {
        if (this.wishIsCurrent) {
          this.$store.dispatch('nextCurrentWish');
        }
      });
    },
    setCurrentWish() {
      this.$store.dispatch('basket/setCurrentWish', {
        gid: this.wish.gid,
        wid: this.wish.id,
      });
      this.$store.dispatch('searchProductsWithName', {
        name: this.wish.name,
      });
    },
    changeQty() {
      this.$store.dispatch('wishlist/product/updateWishProduct', {
        gid: this.wish.gid,
        wid: this.wish.id,
        pid: this.wish.product.id,
        quantity: this.wish.product.quantity,
      });
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
.wish-erase{
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
.total{
  float:left;
  height: 27px;
  line-height: 27px;
  margin: 5px 0 5px 0;
  vertical-align: text-bottom;
}
.count-input {
  position: relative;
  float:left;
  width: 100%;
  max-width: 75px;
  margin: 5px 0;
}
.count-input input {
  width: 100%;
  height: 27px;
  line-height: 27px;
  border: 1px solid #000;
  border-radius: 2px;
  background: none;
  text-align: center;
}
.count-input input:focus {
  outline: none;
}
.count-input .incr-btn {
  display: block;
  position: absolute;
  width: 30px;
  height: 30px;
  font-size: 26px;
  font-weight: 300;
  text-align: center;
  line-height: 30px;
  top: 49%;
  right: 0;
  margin-top: -15px;
  text-decoration:none;
}
input[type=number]::-webkit-inner-spin-button {
  -webkit-appearance: none;
}
.count-input .incr-btn:first-child {
  right: auto;
  left: 0;
  top: 46%;
}
</style>
