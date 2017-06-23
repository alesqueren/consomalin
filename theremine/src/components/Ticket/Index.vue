<template lang="pug">
div#ticket
  h2 Validation de la commande
  .border
  div.explanations(v-if="!isBasketPrepared") 
    i(class="text-center fa fa-spinner fa-spin fa-5x" style="width: 100%;")
    i(class="text-center" style="width: 100%;display: inline-block;") Nous synchronisons votre commande avec Auchan
  div.explanations.alert.alert-warning(v-if="isBasketPrepared")
    div Suite à la syncronisation, des événements on pu survenir.
    div.tip.tip1
      div Stocks insuffisants
      span.deleted XX
      span &nbsp;Il n'y a pas assez de stock
    div.tip.tip2
      div Plus de stock
      span.deleted Produit
      span &nbsp;Le produit n'est plus en stock
    div.tip.tip5
      div Promotion/augmentation
      span.deleted X.XX€
      span &nbsp;Ancien prix
  .box(v-bind:class="{'margintop': !isBasketPrepared}")
    img.logo(src="../../assets/images/logo.jpg")
    .date {{ today }}
    Group.group(v-for="gid in selectedGroups" 
      v-bind:gid="gid" 
      v-bind:key="gid")
    div.doublon(v-if="productInMultipleWish" style="clear:both;") Produits en double
    Product.coucou(v-for="(pid, key, i) in productInMultipleWish"
      v-bind:pid="key" 
      v-bind:key="key")
    .total
      .text TOTAL :
      .amount {{ total }}€
    .payment Retrait à {{ slotFrenchTime }}
    .payment Réglement au retrait
    .thanks Merci de votre visite, à bientôt !
</template>

<script>
import Group from './Group';
import Product from './Product';
import date from '../Utils/date';
import router from '../../router';

export default {
  computed: {
    productInMultipleWish() {
      return this.$store.getters['product/getProductWithMultipleWishes'];
    },
    basketBeforePreparation() {
      return this.$store.state.basket.basketBeforePreparation;
    },
    preparationDiff() {
      return this.$store.state.basket.preparationDiff;
    },
    basketAfterPreparation() {
      return this.$store.state.basket.basketAfterPreparation;
    },
    preparedBasket() {
      return this.$store.state.basket.preparationDiff;
    },
    isBasketPrepared() {
      return this.$store.state.basket.isBasketPrepared;
    },
    priceChanged() {
      const ppTotalPrice = this.preparationDiff.totalPrice;
      const pBpTotalPrice = this.basketBeforePreparation.totalPrice;
      return this.basketPrepared && ppTotalPrice !== pBpTotalPrice;
    },
    details() {
      return this.$store.state.product.details;
    },
    selectedGroups() {
      return this.$store.state.wishGroup.map(group => group.id);
    },
    selectedWishes() {
      return this.$store.getters['selection/getMatchedWishes'];
    },
    selectedSlot() {
      return this.$store.state.singleton.selectedSlot;
    },
    slotFrenchTime() {
      const time = new Date(this.selectedSlot.dateTime);
      const t = date.toFrenchTime(time);
      return t.hours + 'h' + t.minutes + ' le ' + t.dayName + ' ' + t.day + ' ' + t.monthName + ' ' + t.year;
    },
    today() {
      let today = new Date();
      let dd = today.getDate();
      let mm = today.getMonth() + 1;

      const yyyy = today.getFullYear();
      if (dd < 10) {
        dd = '0' + dd;
      }
      if (mm < 10) {
        mm = '0' + mm;
      }
      today = dd + '/' + mm + '/' + yyyy;
      return today;
    },
    total() {
      let res = this.$store.getters['transaction/basketAmount'];
      if (this.isBasketPrepared && this.preparationDiff.totalPrice) {
        res = this.preparationDiff.totalPrice;
      }
      return res;
    },
  },
  created() {
    if (!Object.keys(this.selectedWishes).length) {
      router.push({ name: 'basket' });
    }

    this.$store.dispatch('basket/setIsBasketPrepared', false);

    this.$store.dispatch('basket/prepareOrder').then(() => {
    }, () => {
      router.push({ name: 'withdraw' });
    });
  },
  components: { Group, Product },
};
</script>

<style scoped>
#ticket{
  padding: 30px 65px 30px 65px;
}
.box{
  font-family: receipt;
  position: absolute;
  left: 50%;
  margin-left: -250px;
  width: 500px;
  background-color: white;
  border: 1px dotted black;
  padding: 30px;
}
.margintop{
  margin-top: 45px;
}
.date{
  text-align: center;
  margin-bottom: 15px;
}
h2 {
  text-align: center;
}
.border{
  position: absolute;
  content: '';
  background: var(--color2);
  height: 2px;
  width: 320px;
  margin-left: -160px;
  margin-top: 5px;
  margin-bottom: 30px;
  left: 50%;
}
.logo{
  margin: 30px 0 15px 160px;
}
.total{
  padding-top: 30px;
  clear: both;
  font-size: 1.2em;
  font-weight: bold;
  width: 320px;
  padding-left: 130px;
}
.total .text{
  float: left;
}
.total .amount{
  float: right;
}
.payment{
  clear: both;
}
.group{
  clear: both;
}
.thanks{
  margin-top: 30px;
  text-align: center;
}
.explanations {
  text-align: center;
  height: 125px;
  margin-top: 45px;
}
.tip {
  float: left;
  padding: 5px;
}
.tip1 {
  width: 130px;
}
.tip2 {
  width: 500px;
}
.tip5 {
  width: 122px;
}
.deleted {
  color: var(--danger);
  text-decoration: line-through;
}
.sided {
  position: absolute;
  right: -100px;
}
.promoted {
  color: var(--success);
}
.demoted {
  color: var(--danger);
}
.doublon {
  font-weight: bold;
}
</style>
