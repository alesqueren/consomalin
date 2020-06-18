<template lang="pug">
  div.product-item(
    v-bind:class=" {'active': inCurrentWish}",
    @click="selectProduct")
    div.old(v-if="inBasket && !inCurrentWish")
      span.fa.fa-check &nbsp;&nbsp;&nbsp;
      span Déjà au panier
    img.product-img.center(v-bind:src="product.imageUrl" v-bind:alt="product.name")
    .product-name.center {{product.name}}
    div.bottom
      div.price
        div(style="font-size: 1.3em;") <b> {{price}}&nbsp;€</b>
        div.pu {{priceByQuantity}}€/{{quantityUnit}}
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

        // - TOOO: make a component
        a.incr-btn(@click.prevent.stop='addQuantity(-1)' href="#") –
        input.quantity(type='number', v-model.number='quantity', step='1', value='0', min='1', max='256'
          @click.prevent.stop='',
          disabled="disabled")
        a.incr-btn(@click.prevent.stop='addQuantity(1)' href="#") &plus;

</template>
<script>
import router from '../../router';

export default {
  props: ['pid', 'maxProducts'],
  computed: {
    currentWish() {
      return this.$store.getters['sectionWishes/getCurrent'];
    },
    inBasket() {
      const basket = this.$store.getters['selection/getProductsInBasket'];
      return (basket.indexOf(this.pid) !== -1);
    },
    product() {
      return this.$store.state.product.details[this.pid];
    },
    inCurrentWish() {
      const selection = this.$store.state.selection.basket;
      const products = selection[this.currentWish.gid][this.currentWish.id];
      for (let i = 0; i < products.length; i++) {
        if (products[i].pid === this.pid) {
          return true;
        }
      }
      return false;
    },
    quantity() {
      if (this.inCurrentWish) {
        const product = this.$store.getters['selection/getProduct']({
          gid: this.currentWish.gid,
          wid: this.currentWish.id,
          pid: this.pid,
        });
        return product.quantity;
      }
      return 1;
    },
    price() {
      return this.product.price.toFixed(2);
    },
    priceByQuantity() {
      return this.product.priceByQuantity.toFixed(2);
    },
    quantityUnit() {
      return this.product.quantityUnit;
    },
  },
  methods: {
    selectProduct() {
      if (!this.inCurrentWish) {
        this.$store.dispatch('selection/addProduct', {
          wid: this.currentWish.id,
          pid: this.pid,
          quantity: 1,
        });
      } else {
        this.erase();
      }
    },
    quickSelectProduct() {
      this.$store.dispatch('selection/addProduct', {
        wid: this.currentWish.id,
        pid: this.pid,
        quantity: 1,
      }).then(() => {
        this.$store.dispatch('sectionWishes/next', () => {
          if (!this.$store.getters['sectionWishes/getCurrent']) {
            router.push({ name: 'basket' });
          }
        });
      });
    },
    addQuantity(value) {
      const newQuantity = this.quantity + value;
      if (newQuantity >= 1 && newQuantity <= 64) {
        this.$store.dispatch('selection/updateProduct', {
          wid: this.currentWish.id,
          pid: this.pid,
          quantity: newQuantity,
        });
      } else if (newQuantity === 0) {
        this.erase();
      }
    },
    erase() {
      const wid = this.currentWish.id;
      this.$store.dispatch('selection/removeProduct', { wid, pid: this.pid });
    },
  },
};
</script>

<style scoped>
.product-item {
  background-color: var(--white);
  width:145px;
  height:267px;
  padding: 5px 5px 0 5px;
  float: left;
  /*margin: 5px;*/
  position: relative;
  border: 1px solid #dedede;
  margin-right: -1px;
  margin-bottom: -1px;
  transition: background-color 0.2s;
}

.product-item:not(.active):hover {
  cursor: pointer;
  background-color: var(--color4);
}
.product-item.active:hover {
  cursor: pointer;
}

.active {
  background-color: var(--active);
  transition: background-color 0.2s;
}
.old {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 25px;
  background-color: #5bc0de;
  color: white;
  /*opacity: 0.5;*/
  line-height: 28px;
  text-align: center;
}
.product-img {
  width:133px;
  height:133px;
}
.product-name {
  line-height: 20px;
  padding: 5px;
  height: 60px;
  overflow: hidden;
}
.bottom {
  display: table;
  width: 100%;
  height: 65px;
}
.btns-atb {
  position: absolute;
  right: 0px;
  width: 65px;
}
.price {
  font-size: 1.2em;
  display: table-cell;
  width: 50%;
  vertical-align: middle;
}
.btn-atb {
  height: 34px;
  width: 100%;
  font-size: 1em;
  font-weight: bold;
  cursor: pointer;
  text-align: center;
  padding: 2px;
  line-height: 32px;
  border-radius: 2px;
}
.btn-atb:hover {
  background-color: var(--success);
  transition: background-color 1s;
}
.text-atb {
  line-height: 32px;
}
.pu {
  font-size: 0.9em;
}
.center {
  display: block;
  margin: 0 auto;
}
.count-input {
  display: table-cell;
  width: 50%;
  position: relative;
  vertical-align: middle;
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
  top: 33px;
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
  top: 34px;
}
.tooltip .tooltiptext {
  width: 240px;
}
.tooltip-bottom {
  top: 135%;
  left: 0;
  margin-left: -60px;
}
</style>
