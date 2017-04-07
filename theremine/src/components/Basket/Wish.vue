<template lang='pug'>
  div.wish
    span.fa.fa-eraser.wish-erase(@click.prevent.stop='removeWish($event)')
    span.wish-name(v-if='!productInfos') {{ wish.name }}
    div.product-infos(v-if='productInfos')
      img.product-left(v-bind:src='productInfos.imageUrl')
      div.product-right
        span.product-name {{ wish.name }} <br/>
        span {{productInfos.name}}
        div.product-number
          div.count-input.space-bottom
            a.incr-btn(@click.prevent.stop='decrease' href="#") –
            input.quantity(type='number', v-model.number='quantity', step='1', value='0', min='1', max='256' @click.prevent.stop='')
            a.incr-btn(@click.prevent.stop='increase' href="#") &plus;
          span.total &nbsp;&nbsp;&nbsp;&nbsp;{{total}}€

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
    increase() {
      if (this.productQuantity < 64) {
        const gid = this.wish.gid;
        const wid = this.wish.id;
        const pid = this.productId;
        const quantity = this.productQuantity + 1;
        this.$store.dispatch('wishGroup/setWishProduct', { gid, wid, pid, quantity });
      }
    },
    decrease() {
      if (this.productQuantity > 1) {
        const gid = this.wish.gid;
        const wid = this.wish.id;
        const pid = this.productId;
        const quantity = this.productQuantity - 1;
        this.$store.dispatch('wishGroup/setWishProduct', { gid, wid, pid, quantity });
      }
    },
    focus() {
      this.$refs.editinput.focus();
    },
    remove() {
      this.$store.dispatch('wishGroup/removeWish', {
        wid: this.wid,
      });
    },
  },
};
</script>

<style scoped>
@font-face {
    font-family: gunny;
    src: url('/static/fonts/gnyrwn971.ttf');
}

.wish {
  position: relative;
  min-width: 350px;
  padding: 5px;
  background-color: white;
  border: 1px solid grey;
}

.wish i {
  visibility: hidden;
}

.wish:hover i {
  visibility: visible;
}
.product-infos {
  display: table;
}
.product-left {
  display: table-cell;
}
.product-right {
  position: relative;
  display: table-cell;
  vertical-align: middle;
  text-align: center;
  /*position: absolute;
  right: 20px;
  bottom: 50px;*/
}
.product-number {
  position: absolute;
  left: 0;
  bottom: 10px;
  width: 100%;
}
.product {
  width: 100%;
  max-width: 100%;
}
.wish-name{
  font-family: gunny;
  font-size: 1.5em;
  font-weight: bold;
}
.product-name{
  position: absolute;
  top: 15px;
  width: 75%;
  font-family: gunny;
  font-size: 1.5em;
  font-weight: bold;
}
.wish-erase{
  visibility: hidden;
  position: absolute;
  top: 5px;
  right: 5px;
  color: red;
  z-index: 10;
}
.wish:hover .wish-erase{
  visibility: visible;
}
.total{
  vertical-align: bottom;
  float: right;
  display: block;
  height: 27px;
  line-height: 27px;
  margin: 5px 0 5px 0;
  font-size: 1.5em;
  font-weight: bold;
}
.buttons {
  position: absolute;
  top: 5px;
  right: 5px;
}
.count-input {
  position: relative;
  float: left;
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
