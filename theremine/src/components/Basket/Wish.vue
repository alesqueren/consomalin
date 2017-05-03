<template lang="pug">
  div.wish(
    @click='select()')
    span.wish-name
      span {{ wish.name }}&nbsp;
      span.details(v-if="detailProduct") ({{detailProduct}} / {{ productIds.length }})
    div.wish-erase(
      @click.prevent.stop='erase()')
      span.btnText Décocher&nbsp;
      span.icon.eraseOff.fa.fa-check-square-o.fa-xs
      span.icon.eraseOn.fa.fa-square-o.fa-xs
    span.badge.badge-info.indicator(v-if="badgeLabel") {{badgeLabel}}
    Product(v-for="pid in displayProductIds"
      v-bind:pid="pid",
      v-bind:wid="wid",
      v-bind:key="pid")
    div.emptyBox(v-if='productIds.length === 0')
    span.no-product.fa.fa-hand-pointer-o(v-if="displayNoProduct") &nbsp;Choisir un produit
</template>

<script>
import router from '../../router';
import Product from './Product';

export default {
  props: ['wid', 'displayUnmatchText', 'badgeLabel', 'pid', 'detailProduct'],
  computed: {
    wish() {
      return this.$store.getters['wishGroup/getWish']({ wid: this.wid });
    },
    displayProductIds() {
      return this.pid ? [this.pid] : this.productIds;
    },
    productIds() {
      const products = this.$store.state.selection.basket[this.wish.gid][this.wish.id];
      return products.map(p => p.pid).reverse();
    },
    displayNoProduct() {
      return this.productIds.length === 0 && !this.badgeLabel;
    },
  },
  methods: {
    select() {
      this.$store.dispatch('sectionWishes/set', this.wid).then(() => {
        router.push({ name: 'section' });
      });
    },
    focus() {
      this.$refs.editinput.focus();
    },
    erase() {
      const wid = this.wid;
      const selected = false;
      this.$store.dispatch('selection/selectWish', { wid, selected });
    },
  },
  components: { Product },
};
</script>

<style scoped>
.wish {
  cursor: pointer;
  position: relative;
  float: left;
  height: auto;
  min-width: 320px;
  width: 320px;
  padding: 5px;
  /*background-color: color(--white);*/
  background-color: white;
  border: 1px solid grey;
}
.emptyBox {
  height: 100px;
}
.wish-name{
  font-size: 2em;
  font-weight: bold;
  font-family: learningCurve;
  text-transform: capitalize;
}
.wish-name .details{
  font-family: helvetica;
  font-size: 0.5em;
}
.wish:hover .wish-name{
  text-decoration: underline
}
.wish:hover{
  cursor: pointer;
  background-color: #f0f0f0;
  -webkit-transition: all 0.2s;
  -moz-transition:    all 0.2s;
  -ms-transition:     all 0.2s;
  -o-transition:      all 0.2s;
}
.wish-erase{
  visibility: hidden;
  position: absolute;
  font-size: 1em;
  top: 5px;
  right: 5px;
  z-index: 1;
}
.wish:hover .wish-erase{
/*.wish .wish-erase{*/
  position: absolute;
  border: 1px solid rgba(0,0,0,.01);
  visibility: visible;
  padding: 6px;
  width: 90px;
  height: 33px;
  line-height: 22px;
}
.wish .wish-erase .icon{
  position: absolute;
  top: 10px;
  right: 2px;
}
.wish .wish-erase:hover {
/*.wish .wish-erase {*/
  border: 1px solid rgba(0,0,0,.25);
  border-radius: .25rem;
}
.wish .wish-erase .btnText{
  visibility: hidden;
}
.wish .wish-erase:hover .btnText{
  visibility: visible;
}
.wish .wish-erase .eraseOn{
  visibility: hidden;
}
/*.wish .wish-erase .eraseOn{*/
.wish .wish-erase:hover .eraseOn{
  visibility: visible;
}
/*.wish .wish-erase:hover .eraseOff{*/
.wish .wish-erase .eraseOff{
  visibility: visible;
}
.no-product{
  position: absolute;
  top: 70px;
  left: 100px;
  opacity: 0.2;
}
.wish:hover .no-product{
  opacity: 1;
}
.badge {
  font-size: 1.0em;
  float: right;
  margin: 5px 20px 5px 5px;
}
.noproduct-text {
  font-size: 1.1em;
}
</style>
