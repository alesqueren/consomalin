<template lang="pug">
  div.wish(
    v-bind:class="{'large': badgeLabel}"
    @click='select()')
    //- label(for="coucou")
    input(type="checkbox", checked,
          @click.prevent.stop='erase()')
    span.wish-name
      span {{ wish.name }}&nbsp;
      span.details(v-if="detailProduct") ({{ detailProduct }} / {{ productIds.length }})
    div.wish-erase(
      @click.prevent.stop='erase()')
      span.btnText Je n'en veux plus&nbsp;
      //- span.icon.eraseOff.fa.fa-remove.fa-xs
      span.icon.eraseOn.fa.fa-remove.fa-lg
    span.badge.badge-info.indicator(v-if="badgeLabel", ref="badge") {{badgeLabel}}
    div.wishgroup-name(v-if="displayGroup") {{ group.name }}
    Product(v-for="pid in displayProductIds"
      v-bind:pid="pid",
      v-bind:wid="wid",
      v-bind:key="pid")
    div(v-if='displayProductIds.length === 0 && fillerMessage') {{ fillerMessage }}
    div.emptyBox(v-if='productIds.length === 0 && !fillerMessage')
    span.no-product.fa.fa-hand-pointer-o(v-if="displayNoProduct") &nbsp;Choisir un produit
</template>

<script>
import router from '../../router';
import Product from './Product';

const $ = window.$;

export default {
  props: ['wid', 'displayUnmatchText', 'badgeLabel', 'pid',
    'detailProduct', 'fillerMessage', 'displayGroup'],
  data() {
    return {
      hoverErase: false,
    };
  },
  computed: {
    wish() {
      return this.$store.getters['wishGroup/getWish']({ wid: this.wid });
    },
    group() {
      return this.$store.getters['wishGroup/getGroup']({ gid: this.wish.gid });
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
  created() {
    $(document).on({
      mouseenter: () => {
        this.hoverErase = true;
      },
      mouseleave: () => {
        this.hoverErase = false;
      },
    }, '.wish-erase');
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
  width: 255px;
  padding: 5px;
  background-color: white;
  outline: 1px solid #dedede;
  margin-top: 1px;
  margin-left: 1px;
}
.large{
  width: 320px;
  padding-left: 30px;
}
.emptyBox {
  height: 82px;
}
.wish-name{
  font-size: 1.5em;
  font-weight: bold;
  font-family: learningCurve;
  margin-left: 5px;
  display: block;
  max-height: 31px;
  overflow: hidden;

  /*text-transform: capitalize;*/
}
.wishgroup-name{
  font-size: 1.5em;
  font-family: learningCurve;
  margin-left: 5px;
  /*text-transform: capitalize;*/
}
.wish-name .details{
  font-family: helvetica;
  font-size: 0.5em;
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
  top: -1px;
  right: -1px;
  z-index: 1;
}
.wish:hover .wish-erase{
/*.wish .wish-erase{*/
  position: absolute;
  border: 1px solid rgba(0,0,0,.01);
  visibility: visible;
  padding: 2px;
  width: 120px;
  height: 25px;
  line-height: 25px;
  font-size: 12px;
  border-top-left-radius: 2px;
  border-bottom-left-radius: 2px;
  border-bottom-right-radius: 2px;
}
.wish .wish-erase .icon{
  position: absolute;
  top: 7px;
  right: 7px;
}
.wish .wish-erase:hover {
/*.wish .wish-erase {*/
  border: 1px solid rgba(0,0,0,.25);
  background-color: #f0f0f0;
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
.wish:hover .eraseOn{
  visibility: visible;
}
/*.wish .wish-erase:hover .eraseOff{*/
.wish .wish-erase .eraseOff{
  visibility: visible;
}
.wish .wish-erase:hover .eraseOff{
  visibility: visible;
  /*visibility: hidden;*/
}
.no-product{
  position: absolute;
  top: 60px;
  left: 75px;
  opacity: 0.2;
  transition: opacity 0.2s;
}
.wish:hover .no-product{
  transition: opacity 0.2s;
  opacity: 1;
}
.badge {
  font-size: 0.9em;
  float: right;
  margin: 5px 20px 5px 5px;
  padding-top: 8px;
}
.noproduct-text {
  font-size: 1.1em;
}
[type="checkbox"]:not(:checked),
[type="checkbox"]:checked {
  position: absolute;
  left: -9999px;
}

[type="checkbox"]:not(:checked) + label,
[type="checkbox"]:checked + label {
  position: relative;
  padding-left: 35px;
}
[type="checkbox"]:not(:checked) + label:before,
[type="checkbox"]:checked + label:before {
  content: '';
  position: absolute;
  left:-30px; top: 12px;
  width: 25px; height: 25px;
  border: 1px solid #aaa;
  background: #f8f8f8;
  border-radius: 3px;
  box-shadow: inset 0 1px 3px rgba(0,0,0,.3);
}
[type="checkbox"]:not(:checked):disabled + label:before,
[type="checkbox"]:checked:disabled + label:before {
  box-shadow: inset 0 1px 28px rgba(0,0,0,.3);
}

[type="checkbox"]:not(:checked):not(:disabled) + label:after,
[type="checkbox"]:checked:not(:disabled) + label:after {
  content: '✔';
  position: absolute;
  top: 2px; left: -30px;
  font-size: 30px;
  color: #09ad7e;
  transition: all .1s;
}

[type="checkbox"]:not(:checked) + label:after {
  opacity: 0;
  transform: scale(0);
}
[type="checkbox"]:checked + label:after {
  opacity: 1;
  transform: scale(1);
}
</style>
