<template lang="pug">
  div.product.full(
    @click='select()'
    v-if="productInfos")
    div.product-infos
      span.fa.fa-trash-o.fa-xs.product-erase(@click.prevent.stop='erase()')
      img.product-left(v-bind:src='productInfos.imageUrl')
      div.product-right
        div {{productInfos.name}}
        div.product-number
          div.count-input.space-bottom
            a.incr-btn(@click.prevent.stop='decrease' href="#") –
            //- div.erase(v-if="eraseEdition", @click.prevent.stop='erase()' href="#") Supprimer
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
      eraseEdition: false,
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
      } else {
        this.eraseStart();
      }
    },
    focus() {
      this.$refs.editinput.focus();
    },
    eraseStart() {
      this.eraseEdition = true;
    },
    eraseStop() {
      this.eraseEdition = false;
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
  height: auto;
  width: 100%;
  /*position: absolute;
  right: 20px;
  bottom: 50px;*/
}
.product-number {
  position: relative;
  display: table;
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
  display: table-cell;
  width: 50%;
  height: 27px;
  line-height: 27px;
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
  display: table-cell;
  width: 60px;
  max-width: 75px;
}
.count-input input {
  position: relative;
  width: 60px;
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
  right: 15px;
  margin-top: -15px;
  text-decoration:none;
}
input[type=number]::-webkit-inner-spin-button {
  -webkit-appearance: none;
}
.count-input .incr-btn:first-child {
  right: auto;
  left: 15px;
  z-index: 2;
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
.erase{
  position: absolute;
  font-size: 1.5em;
  top: 5px;
  right: 5px;
  color: white;
  z-index: 2;
  background-color: red;
}
.product:hover .product-erase{
  visibility: visible;
}
.product .product-erase:hover{
  color: red;
}
</style>
