<template lang="pug">
  div.product(
    v-if="productInfos")
    div.productName(v-bind:class="{'deleted': productDeleted}")
      span.newQuantity(v-if="productPartiallyDeleted") {{pp.productNb}}
      span.quantity(v-bind:class="{'partiallyDeleted': productPartiallyDeleted}") {{pBp.productNb}}
      span x{{productInfos.name}}
    span.total.newTotal(
    v-if="totalChange"
      v-bind:class="{'demoted': productDemoted, 'promoted': productPromoted}"
      ) &nbsp;&nbsp;&nbsp;&nbsp;{{pp.price}}€
    span.total(v-bind:class="{'partiallyDeleted': productDemoted || productPromoted || totalChange}") &nbsp;&nbsp;&nbsp;&nbsp;{{pBp.price}}€

</template>

<script>

export default {
  props: ['wid', 'pid'],
  computed: {
    basketBeforePreparation() {
      return this.$store.state.basket.basketBeforePreparation;
    },
    productBeforePreparation() {
      let result = {};
      if (this.basketBeforePreparation && this.basketBeforePreparation) {
        result = this.basketBeforePreparation[this.pid];
      }
      return result;
    },
    pBp() {
      return this.productBeforePreparation;
    },
    preparedBasket() {
      return this.$store.state.basket.preparedBasket;
    },
    basketPrepared() {
      return Object.keys(this.preparedBasket).length;
    },
    preparedProduct() {
      let result = {};
      if (this.preparedBasket && this.preparedBasket.products) {
        result = this.preparedBasket.products[this.pid];
      }
      return result;
    },
    pp() {
      return this.preparedProduct;
    },
    productDeleted() {
      let result = false;
      if (this.pp.productNb === 0) {
        result = true;
      }
      return result;
    },
    productPartiallyDeleted() {
      let result = false;
      if (this.pp.productNb < this.pBp.productNb) {
        result = this.preparedProduct.productNb;
      }
      return result;
    },
    productPromoted() {
      let result = false;
      if (this.basketPrepared && this.pp.priceByProduct < this.pBp.priceByProduct) {
        result = true;
      }
      return result;
    },
    productDemoted() {
      let result = false;
      if (this.basketPrepared && this.pp.priceByProduct > this.pBp.priceByProduct) {
        result = true;
      }
      return result;
    },
    quantity() {
      const wish = this.$store.getters['wishGroup/getWish']({ wid: this.wid });
      const product = this.$store.getters['selection/getProduct']({
        gid: wish.gid,
        wid: this.wid,
        pid: this.pid,
      });
      return product.quantity;
    },
    productInfos() {
      return this.$store.state.product.details[this.pid];
    },
    totalChange() {
      let result = false;
      if (this.basketPrepared && this.pp.price !== this.pBp.price) {
        result = true;
      }
      return result;
    },
  },
};
</script>

<style scoped>
.product {
  position: relative;
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
.promoted {
  color: var(--danger);
}
.demoted {
  color: var(--success);
}
</style>
