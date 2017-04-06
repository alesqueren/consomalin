<template lang='pug'>
transition(name="fade")
  .wish.list-group-item(v-bind:class='{active:wishIsCurrent}', @click='setCurrentWish()')
    span.fa.fa-eraser.wish-erase(@click.prevent.stop='removeWish($event)')
    div
      span.wishgroupname.badge.badge-success {{wish.gname}}
      span.wishname {{wish.name}}
    div(v-if='hasProduct')
      div  {{productDetails.name}}
      |  
      img(style='width:150px;', v-bind:src='productDetails.imageUrl')
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
  props: ['wid'],
  computed: {
    quantity: {
      get() {
        return this.productQuantity.quantity;
      },
      set(quantity) {
        this.$store.dispatch('wishGroup/setWishProduct', {
          gid: this.wish.gid,
          wid: this.wish.id,
          pid: this.wish.product.id,
          quantity,
        });
      },
    },
    wish() {
      return this.$store.getters['wishGroup/getWish'](this.wid);
    },
    hasProduct() {
      return this.$store.getters['selection/getMatchedWishes'][this.wid];
    },
    productId() {
      if (this.hasProduct) {
        return this.$store.getters['selection/getMatchedWishes'][this.wid].pid;
      }
      return null;
    },
    productQuantity() {
      if (this.hasProduct) {
        return this.$store.getters['selection/getMatchedWishes'][this.wid].quantity;
      }
      return null;
    },
    productDetails() {
      if (this.hasProduct) {
        return this.$store.state.product.details[this.productId];
      }
      return null;
    },
    wishIsCurrent() {
      const currentWishId = this.$store.state.singleton.currentWishId;
      return this.wish.id === currentWishId;
    },
    total() {
      let total = 0;
      if (this.hasProduct) {
        total = this.productDetails.price * this.productQuantity;
      }
      return parseFloat(total).toFixed(2);
    },
  },
  methods: {
    increase() {
      if (this.productQuantity < 64) {
        const gid = this.wish.gid;
        const wid = this.wish.id;
        const pid = this.productId;
        const quantity = this.productQuantity + 1;
        this.$store.dispatch('wishGroup/setWishProduct', {
          gid,
          wid,
          pid,
          quantity,
        });
      }
    },
    decrease() {
      if (this.productQuantity > 1) {
        const gid = this.wish.gid;
        const wid = this.wish.id;
        const pid = this.productId;
        const quantity = this.productQuantity - 1;
        this.$store.dispatch('wishGroup/setWishProduct', {
          gid,
          wid,
          pid,
          quantity,
        });
      }
    },
    removeWish() {
      const wid = this.wish.id;
      const selected = false;
      this.$store.dispatch('wishGroup/selectWish', { wid, selected }).then(() => {
        if (this.wishIsCurrent) {
          this.$store.dispatch('currentWish/next');
        }
      });
    },
    setCurrentWish() {
      this.$store.dispatch('singleton/set', {
        key: 'currentWishId',
        wid: this.wid,
      });
      this.$store.dispatch('product/fetchSearch', this.wish.name);
    },
    changeQty() {
      this.$store.dispatch('wishGroup/setWishProduct', {
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
