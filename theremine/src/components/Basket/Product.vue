<template lang="pug">
  div.product.full(
    @click='select()'
    v-if="productInfos")
    div.product-infos
      span.fa.fa-trash-o.fa-xs.product-erase(@click.prevent.stop='erase()')
      img.product-left(v-bind:src='productInfos.imageUrl')
      div.product-right
        span {{productInfos.name}}
        div.product-number
          div.count-input.space-bottom
            a.incr-btn(@click.prevent.stop='decrease' href="#") –
            input.quantity(type='number', v-model.number='quantity', step='1', value='0', min='1', max='256' @click.prevent.stop='', disabled="disabled")
            a.incr-btn(@click.prevent.stop='increase' href="#") &plus;
          span.total &nbsp;&nbsp;&nbsp;&nbsp;{{total}}€

</template>

<script>
import router from '../../router';

export default {
  props: ['wid', 'pid'],
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
        const pid = this.pid;
        this.$store.dispatch('updateWishProduct', { gid, wid, pid, quantity });
      },
    },
    wish() {
      return this.$store.getters['wishGroup/getWish']({ wid: this.wid });
    },
    productQuantity() {
      return this.$store.state.selection[this.wish.gid][this.wish.id][this.pid];
    },
    productInfos() {
      return this.$store.state.product.details[this.pid];
    },
    total() {
      const total = this.productInfos.price * this.productQuantity;
      return parseFloat(total).toFixed(2);
    },
  },
  methods: {
    select() {
      this.$store.dispatch('currentWish/set', {
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
        const pid = this.pid;
        const quantity = parseInt(this.productQuantity + 1, 10);
        this.$store.dispatch('wishGroup/updateWishProduct', { wid, pid, quantity });
      }
    },
    decrease() {
      if (this.productQuantity > 1) {
        const wid = this.wish.id;
        const pid = this.pid;
        const quantity = parseInt(this.productQuantity - 1, 10);
        this.$store.dispatch('wishGroup/updateWishProduct', { wid, pid, quantity });
      }
    },
    focus() {
      this.$refs.editinput.focus();
    },
    erase() {
      const wid = this.wid;
      this.$store.dispatch('wishGroup/removeWishProduct', { wid, pid: this.pid });
    },
  },
};
</script>

<style scoped>
.product {
  cursor: pointer;
  position: relative;
  height: auto;
  min-width: 315px;
  width: 315px;
  padding: 10px;
}
.wish:hover .product-name{
  text-decoration: underline;
}

.product-infos {
  display: table;
}
.product-left {
  display: table-cell;
  width: 100px;
}
.product-right {
  position: relative;
  display: table-cell;
  vertical-align: middle;
  text-align: center;
  height: 125px;
  width: 100%;
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
.product-name{
  position: absolute;
  top: 15px;
  width: 75%;
  font-family: gunny;
  font-size: 1.5em;
  font-weight: bold;
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

.product-erase{
  visibility: hidden;
  position: absolute;
  font-size: 1.5em;
  top: 5px;
  right: 5px;
  color: #555;
  z-index: 1;
}
.product:hover .product-erase{
  visibility: visible;
}
.product .product-erase:hover{
  color: red;
}
</style>
