<template lang="pug">
  div.product(
    v-if="productInfos")
    div.productName(v-bind:class="{'deleted': productDeleted}")
      span.newQuantity(v-if="productPartiallyDeleted") {{productNb}}
      span.quantity(v-bind:class="{'quantityReduced': productPartiallyDeleted}") {{productNb}}
      span x{{productInfos.name}}
    span.total.newTotal(
    v-if="totalChange && productDemoted || productPromoted"
      v-bind:class="{'demoted': productDemoted, 'promoted': productPromoted}"
      ) &nbsp;&nbsp;&nbsp;&nbsp;{{pp.price}}€
    span.total(v-bind:class="{'oldPrice': productDemoted || productPromoted, 'partiallyDeleted': totalChange, 'deleted': productDeleted}") {{pBp.price}}€

</template>

<script>

export default {
  props: ['pid'],
  computed: {
    productBeforePreparation() {
      return this.$store.state.basket.basketBeforePreparation.products[this.pid];
    },
    pBp() {
      return this.productBeforePreparation;
    },
    pBpProductNb() {
      return this.pBp ? this.pBp.productNb : 0;
    },
    preparedBasket() {
      return this.$store.state.basket.preparationDiff;
    },
    basketIsPrepared() {
      return this.$store.state.basket.isBasketPrepared;
    },
    productAfterPreparation() {
      const basketAfterPreparation = this.$store.state.basket.basketAfterPreparation;
      const products = basketAfterPreparation ? basketAfterPreparation.products : null;
      const product = products ? products[this.pid] : null;
      return product;
    },
    pp() {
      return this.productAfterPreparation;
    },
    ppProductNb() {
      return this.pp ? this.pp.productNb : 0;
    },
    productDeleted() {
      return this.basketIsPrepared ? !this.pp : false;
    },
    productPartiallyDeleted() {
      let result = false;
      if (this.basketIsPrepared && this.pp && this.pBp && this.pp.productNb < this.pBp.productNb) {
        result = this.pp.productNb;
      }
      return result;
    },
    productNb() {
      let result = this.pBpProductNb;
      if (this.basketIsPrepared && this.pp && this.pp.productNb) {
        result = this.pp.productNb;
      }
      return result;
    },
    productPrice() {
      let result = this.price;
      if (this.basketIsPrepared && this.pp && this.pp.price) {
        result = this.pp.price;
      }
      return result;
    },
    productPromoted() {
      let result = false;
      if (this.basketIsPrepared && this.pp && this.pBp.priceByProduct > this.pp.priceByProduct) {
        result = true;
      }
      return result;
    },
    productDemoted() {
      let result = false;
      try {
        if (this.basketIsPrepared && this.pp && this.pBp.priceByProduct < this.pp.priceByProduct) {
          result = true;
        }
      } catch (e) {
        return false;
      }
      return result;
    },
    quantity() {
      const quantity = this.$store.getters['product/getTotalQuantity']({ pid: this.pid });
      return quantity;
    },
    productInfos() {
      return this.$store.state.product.details[this.pid];
    },
    totalChange() {
      let result = false;
      const bp = this.basketIsPrepared;
      try {
        if (bp && this.pp.price && this.pBp && this.pp.price !== this.pBp.price) {
          result = true;
        }
      } catch (e) {
        return false;
      }
      return result;
    },
  },
};
</script>

<style scoped>
.product {
  position: relative;
  clear: both;
}
.productPrepared {
  position: absolute;
  right: 120px;
}
.productName {
  float: left;
  width: 340px;
}
.total {
  float: right;
}
.deleted {
  color: var(--danger);
  text-decoration: line-through;
}
.newQuantity {
  color: var(--danger);
}
.partiallyDeleted {
  color: var(--danger);
  text-decoration: line-through;
}
.quantityReduced {
  color: var(--danger);
  text-decoration: line-through;
  position: absolute;
  left: -50px;
}
.oldPrice {
  position: absolute;
  right: -100px;
  color: var(--danger);
  text-decoration: line-through;
}
.promoted {
  color: var(--success);
}
.demoted {
  color: var(--danger);
}
</style>
