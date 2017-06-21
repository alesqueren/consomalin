<template lang="pug">
  div.product(
    v-if="productInfos")
    div.productName(v-bind:class="{'deleted': productDeleted}")
      span.newQuantity(v-if="productPartiallyDeleted") {{pp.productNb}}
      span.quantity(v-bind:class="{'quantityReduced': productPartiallyDeleted}") {{pBp.productNb}}
      span x{{productInfos.name}}
    span.total.newTotal(
    v-if="totalChange && productDemoted || productPromoted"
      v-bind:class="{'demoted': productDemoted, 'promoted': productPromoted}"
      ) &nbsp;&nbsp;&nbsp;&nbsp;{{pp.price}}€
    span.total(v-bind:class="{'oldPrice': productDemoted || productPromoted, 'partiallyDeleted': totalChange}") {{pBp.price}}€

</template>

<script>

export default {
  props: ['pid'],
  computed: {
    productBeforePreparation() {
      return this.$store.state.basket.basketBeforePreparation[this.pid];
    },
    pBp() {
      return this.productBeforePreparation;
    },
    preparedBasket() {
      return this.$store.state.basket.preparationDiff;
    },
    basketIsPrepared() {
      return this.$store.state.basket.isBasketPrepared;
    },
    preparedProduct() {
      let result = {};
      if (this.basketIsPrepared && this.preparedBasket.products) {
        result = this.preparedBasket.products[this.pid];
      }
      console.log('pid : ' + this.pid);
      console.log(result);
      return result;
    },
    pp() {
      return this.preparedProduct;
    },
    productDeleted() {
      let result = false;
      if (this.basketIsPrepared && this.pp && this.pp.productNb === 0) {
        result = true;
      }
      return result;
    },
    productPartiallyDeleted() {
      let result = false;
      if (this.basketIsPrepared && this.pp && this.pp.productNb < this.pBp.productNb) {
        result = this.preparedProduct.productNb;
      }
      return result;
    },
    productPromoted() {
      let result = false;
      try {
        if (this.basketIsPrepared && this.pBp.priceByProduct > this.pp.priceByProduct) {
          result = true;
        }
      } catch (e) {
        return false;
      }
      return result;
    },
    productDemoted() {
      let result = false;
      try {
        if (this.basketIsPrepared && this.pBp.priceByProduct < this.pBp.priceByProduct) {
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
      try {
        if (this.basketIsPrepared && this.pp.price !== this.pBp.price) {
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
