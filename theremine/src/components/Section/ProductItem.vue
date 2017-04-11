<template lang='pug'>
  div.product-item
    img.product-img.center(v-bind:src="product.imageUrl")
    .product-name.center {{product.name}}
    div.count-input
      a.incr-btn(@click.prevent.stop='decrease' href="#") –
      input.quantity(type='number', v-model.number='quantity', step='1', value='0', min='1', max='256' @click.prevent.stop='')
      a.incr-btn(@click.prevent.stop='increase' href="#") &plus;
    div.price
      div <b>{{product.price}}&nbsp;€</b>
      div.pu {{product.priceByQuantity}}&nbsp;€/u
    div.btn-atb(@click="selectProduct()")
      i.fa.fa-shopping-basket.fa-xs.text-atb &nbsp;&nbsp;&nbsp;&nbsp;Ajouter au panier
</template>

<script>
export default {
  props: ['pid', 'maxProducts'],
  data() {
    return {
      quantity: '1',
    };
  },
  computed: {
    product() {
      return this.$store.state.product.details[this.pid];
    },
    currentWishId() {
      return this.$store.state.singleton.currentWishId;
    },
    currentWish() {
      return this.$store.getters['wishGroup/getWish']({ wid: this.currentWishId });
    },
  },
  methods: {
    selectProduct() {
      this.$store.dispatch('wishGroup/setWishProduct', {
        gid: this.currentWish.gid,
        wid: this.currentWish.id,
        pid: this.pid,
        quantity: this.quantity,
      });
      this.$store.dispatch('currentWish/next');
    },
    increase() {
      if (this.quantity < 64) {
        this.quantity++;
      }
    },
    decrease() {
      if (this.quantity > 1) {
        this.quantity--;
      }
    },
  },
};
</script>

<style scoped>
.product-item{
  background-color: white;
  border: 1px solid rgba(0,0,0,.125);
  width:162px;
  height:275px;
  padding: 5px 5px 0 5px;
  float: left;
  margin: 5px;
}
.product-img{
  width:150px;
  height:150px;
}
.product-name{
  height: 50px;
}
.btn-atb{
  clear: both;
  font-size: 1em;
  font-weight: bold;
  background-color: #5bc0de;
  cursor: pointer;
  text-align: center;
  padding: 2px;
  width: 163px;
  margin-left: -6px;
  height: 32px;
  line-height: 32px;
}
.text-atb{
  line-height: 32px;
}
.price{
  font-size: 1.2em;
  float: right;
}
.pu{
  font-size: 0.7em;
}
.center{
  display: block;
  margin: 0 auto;
}
.count-input {
  position: relative;
  float: left;
  width: 100%;
  max-width: 65px;
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
