<template lang="pug">
  div.product-item(
    v-bind:class="{'active': inCurrentWish}", 
    @click="selectProduct")
    div.old(v-if="inCurrentBasket && !inCurrentWish")
      span.fa.fa-check &nbsp;&nbsp;&nbsp;
      span Déjà au panier
    img.product-img.center(v-bind:src="product.imageUrl")
    .product-name.center {{product.name}}
    div.bottom
      div.price
        div(style="font-size: 1.3em;") <b>{{product.price}}&nbsp;€</b>
        div.pu {{product.priceByQuantity}}&nbsp;€/u
      div.btns-atb(v-if="!inCurrentWish")
        div.btn-atb.tooltip(
            @click.stop="selectProduct",
          )
          span.tooltiptext.tooltip-bottom Ajouter au panier
          i.fa.fa-cart-plus.fa-lg.atb
        div.btn-atb.tooltip(
            @click.stop="quickSelectProduct()",
          )
          span.tooltiptext.tooltip-bottom Ajouter au panier et<br> passer au produit suivant 
          i.fa.fa-cart-plus.fa-lg.atb-quick &nbsp;
          span.fa.fa-arrow-right.special-fa
      div.count-input.space-bottom(v-else)
        a.incr-btn(@click.prevent.stop='decrease' href="#") –
        input.quantity(type='number', v-model.number='quantity', step='1', value='0', min='1', max='256'
          @click.prevent.stop='',
          disabled="disabled")
        a.incr-btn(@click.prevent.stop='increase' href="#") &plus;
</template>
<script>
import router from '../../router';

export default {
  props: ['pid', 'maxProducts'],
  data() {
    return {
      tmpSelectedProduct: false,
    };
  },
  computed: {
    currentWish() {
      return this.$store.getters['sectionWishes/getCurrent'];
    },
    inCurrentBasket() {
      return this.$store.getters['selection/getProductsInBasket'][this.pid];
    },
    product() {
      return this.$store.state.product.details[this.pid];
    },
    inCurrentWish() {
      const selection = this.$store.state.selection;
      const pids = Object.keys(selection[this.currentWish.gid][this.currentWish.id]);
      return pids && pids.indexOf(this.pid) !== -1;
    },
    quantity() {
      const product = this.$store.state.selection[this.currentWish.gid][this.currentWish.id];
      return product[this.pid] || 1;
    },
    deleting() {
      const action = this.$store.state.singleton.action;
      const wid = action.wid;
      const pid = action.pid;
      const type = action.type;
      return type === 'deleteProduct' && this.wid === wid && this.pid === pid;
    },
  },
  methods: {
    selectProduct() {
      const products = [{
        pid: this.pid,
        quantity: parseInt(this.quantity, 10),
      }];
      this.$store.dispatch('selection/setWishProducts', {
        wid: this.currentWish.id,
        products,
      });
    },
    quickSelectProduct() {
      const products = [{
        pid: this.pid,
        quantity: parseInt(this.quantity, 10),
      }];
      this.$store.dispatch('selection/setWishProducts', {
        wid: this.currentWish.id,
        products,
      }).then(() => {
        this.$store.dispatch('sectionWishes/next', () => {
          if (!this.$store.getters['sectionWishes/getCurrent']) {
            router.push({ name: 'basket' });
          }
        });
      });
    },
    increase() {
      if (this.quantity < 64) {
        const wid = this.currentWish.id;
        const pid = this.pid;
        const quantity = parseInt(this.quantity + 1, 10);
        this.$store.dispatch('selection/updateWishProduct', { wid, pid, quantity });
      }
    },
    decrease() {
      if (this.quantity > 1) {
        const wid = this.currentWish.id;
        const pid = this.pid;
        const quantity = parseInt(this.quantity - 1, 10);
        this.$store.dispatch('selection/updateWishProduct', { wid, pid, quantity });
      }
    },
  },
};
</script>

<style scoped>
.product-item {
  background-color: var(--white);
  border: 1px solid rgba(0,0,0,.125);
  width:162px;
  height:275px;
  padding: 5px 5px 0 5px;
  float: left;
  margin: 5px;
  position: relative;
}

.product-item:not(.active):hover {
  cursor: pointer;
  background-color: var(--color4);
}

.active{
  background-color: var(--active);
}
.old{
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 25px;
  background-color: #5bc0de;
  /*color: white;*/
  /*opacity: 0.5;*/
  line-height: 25px;
  text-align: center;
}
.product-img{
  width:150px;
  height:150px;
}
.product-name{
  height: 50px;
}
.bottom{
  display: table;
  width: 100%;
  height: 65px;
}
.btns-atb{
  display: table-cell;
  width: 50%;
}
.price{
  font-size: 1.2em;
  display: table-cell;
  width: 50%;
  vertical-align: middle;
}
.btn-atb{
  font-size: 1em;
  font-weight: bold;
  cursor: pointer;
  text-align: center;
  padding: 2px;
  height: 32px;
  line-height: 32px;
}
.btn-atb:hover{
  background-color: var(--success);
}
.text-atb{
  line-height: 32px;
}
.pu{
  font-size: 0.9em;
}
.center{
  display: block;
  margin: 0 auto;
}
.count-input {
  display: table-cell;
  width: 50%;
  position: relative;
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
  top: 14px;
  right: 0;
  margin-top: -15px;
  text-decoration:none;
}
.count-input .incr-btn:hover {
  font-size: 30px;
}
input[type=number]::-webkit-inner-spin-button {
  -webkit-appearance: none;
}
.count-input .incr-btn:first-child {
  right: auto;
  left: 0;
  top: 12px;
}
.tooltip .tooltiptext {
  width: 240px;
}
.tooltip-bottom{
  top: 135%;
  left: 0;
  margin-left: -60px;
}
</style>
