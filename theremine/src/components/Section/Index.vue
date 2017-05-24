<template lang="pug">
  div#wishes
    div.filler(v-if="!currentWish && isBasketEmpty")
      span Ta liste de course est vide.
      div
        img(src="https://www.oppermann.fr/387892-61468-thickbox/bloc-notes-et-stylo-recycles.jpg")
    div.filler(v-else-if="!currentWish && !isBasketEmpty")
      span Le panier est plein.
      div
        img(src="https://i.skyrock.net/2347/38502347/pics/3189543713_1_6_3ubLwyfJ.gif")
    div(v-else) 
      div.leftSide
        CurrentWish
        div(v-if="currentWishResults")
          ProductItem(
            v-for="(pid, key, i) in currentWishResults" 
            v-if="i < maxProducts"
            v-bind:maxProducts="maxProducts" 
            v-bind:pid="pid" 
            v-bind:key="i")
        div.waiting-box(
          v-if="!currentWishResults"
          v-for="i in 40"
          v-bind:key="i")
          img.product-img.center
          .product-name.center
          div.count-input.space-bottom
            a.incr-btn(href="#") –
            input.quantity(type='number', value='1' disabled="disabled")
            a.incr-btn(href="#") &plus;
          div.price
          div.btn-atb
            i.fa.fa-shopping-basket.fa-xs.text-atb &nbsp;&nbsp;&nbsp;&nbsp;Ajouter au panier

      div.nothing-box(v-if="currentWishResults && !currentWishResults[0]")
        div.filler(style="width: 100%; text-align: center;")
          span Aucun produit trouvé.
          br
          span Vous pouvez modifier la recherche dans la barre ci dessus.
          br

      RightBar.rightSide

</template>

<script>
import config from '../../../config';
import CurrentWish from './CurrentWish';
import ProductItem from './ProductItem';
import RightBar from './RightBar';
import List from '../List/Index';
import router from '../../router';

const $ = window.$;

export default {
  data() {
    return {
      maxProducts: 40,
      demo: config.MODE_DEMO,
    };
  },
  destroyed() {
    window.removeEventListener('scroll', this.handleScroll);
  },
  watch: {
    currentWish: () => {
      // scroll to top
      window.$('html,body').scrollTop(0);
    },
  },
  computed: {
    searchs() {
      return this.$store.state.searchs;
    },
    currentWish() {
      return this.$store.getters['sectionWishes/getCurrent'];
    },
    total() {
      return this.$store.getters['transaction/basketAmount'];
    },
    isBasketEmpty() {
      return this.$store.getters['selection/getOrderedSelectedWishes'].length === 0;
    },
    selectedWishNb() {
      return this.$store.getters['selection/getOrderedSelectedWishes'].length;
    },
    matchedWishesLength() {
      return Object.keys(this.$store.getters['selection/getMatchedWishes']).length;
    },
    currentWishResults() {
      if (this.currentWish && this.currentWish.name) {
        return this.$store.state.product.searchs[this.currentWish.name];
      }
      return [];
    },
  },
  methods: {
    // lazyloading
    // lorsqu'on atteint le bas de la page a 100px pret on augmente le nombre de produits affichable
    handleScroll() {
      const scrollTop = $(window).scrollTop();
      const height = $(window).height();
      const searchsRes = this.$store.state.product.searchs[this.currentWish.name];
      const nbResult = searchsRes ? Object.keys(searchsRes).length : 0;
      if (scrollTop + height > height - 100 && this.maxProducts < nbResult) {
        this.maxProducts += 20;
      }
    },
  },
  created() {
    window.addEventListener('scroll', this.handleScroll);
    if (!this.selectedWishNb) {
      router.push({ name: 'wishlist' });
    }
  },
  mounted() {
    if (this.demo && window.innerWidth > 1200) {
      $('#wishes .rightSide').css('top', '+=50px');
    }
  },
  components: { CurrentWish, ProductItem, List, RightBar },
};
</script>

<style scoped>

.filler {
  font-size: 1.7em;
  width: 100%;
  text-align: center;
}
.filler img {
  height: 200px;
  margin-top: 100px;
}
#wishes{
  width: 100%;
  padding: 30px 65px 30px 65px;
}
#wishes .main{
}
#wishes .leftSide{
  vertical-align: top;
  /*padding-right: 320px;*/
}
#wishes .rightSide{
  position: fixed;
  top: 158px;
  right: 16px;
  width: 334px;
}
@media screen and (max-width: 1200px) {
  #wishes .rightSide {
    position: absolute;
    right: -304px;
    top: 115px;
  }
}
.waiting{
  background-color: color(--white);
  width: 150px;
  height: 150px;
}
.waiting-box{
  background-color: #eceeef;
  border: 1px solid rgba(0,0,0,.125);
  width: 162px;
  height: 275px;
  padding: 5px 5px 0 5px;
  float: left;
  opacity: 0.3;
}
.product-img{
  background-color: color(--white);
  width:150px;
  height:150px;
}
.product-name{
  height: 50px;
}
.btn-atb{
  clear: both;
  font-size: 1em;
  font-weight: bold;
  background-color: color(--white);
  cursor: pointer;
  text-align: center;
  padding: 2px;
  width: 160px;
  margin-left: -5px;
  height: 32px;
  line-height: 32px;
}
.text-atb{
  line-height: 32px;
}
.nothing-box{
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
.nav-btn {
  position: relative;
  width: auto;
  height: 47px;
  cursor: pointer;
  text-align: center;
  color: var(--white);
  font-weight: bolder;
  background-color: var(--success);
}
.nav-btn:hover {
  background-color: var(--color3-3);
}
</style>
