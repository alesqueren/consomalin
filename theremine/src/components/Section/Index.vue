<template lang='pug'>
  div#wishes
    CurrentWish
    div(v-if="!currentWishIsEmpty && currentWishResults")
      ProductItem(
        v-for="(pid, key, i) in currentWishResults" 
        v-if="i < maxProducts"
        v-bind:maxProducts="maxProducts" 
        v-bind:pid="pid" 
        v-bind:key="i")
    div.waiting-box(
      v-if="!currentWishIsEmpty && !currentWishResults"
      v-for="i in 40"
      v-bind:key="i")
      img.product-img.center
      .product-name.center
      div.count-input
        input.quantity(type='number', step='1', value='0', min='1', max='256')
      div.price
      div.btn-atb
        i.fa.fa-shopping-basket.fa-xs.text-atb &nbsp;&nbsp;&nbsp;&nbsp;Ajouter au panier
    div.nothing-box(v-if="!currentWishIsEmpty && currentWishResults && !currentWishResults[0]")
      span Aucun produits ne correspond à cette recherche
    .container(v-else-if="basket.length === 0")
      div
        span Vous n'avez choisi aucun produit, ajoutez-en dans vos 
          router-link(:to='{ name: "wishlist" }')
            button.btn(v-bind:class="nextInfos.class" type="button") listes de courses
    .container(v-else-if="basketFull")
        div
          span Votre liste de course est complète ! Vous pouvez 
            router-link(:to='{ name: "basket" }')
              button.btn(v-bind:class="nextInfos.class" type="button") Passer au panier 
          span  pour finaliser la commande.
</template>

<script>
import CurrentWish from './CurrentWish';
import ProductItem from './ProductItem';

export default {
  data() {
    return {
      maxProducts: 40,
    };
  },
  created() {
    // on ecoute le scroll pour augmenter le nombre de produits visibles
    // document.addEventListener('scroll',CheckIfScrollBottom);
  },
  destroyed() {
    // window.removeEventListener('scroll', this.handleScroll);
    // document.removeEventListener('scroll',CheckIfScrollBottom);
  },
  computed: {
    currentWishId() {
      return this.$store.state.singleton.currentWishId;
    },
    currentWish() {
      return this.$store.getters['wishGroup/getWish']({ wid: this.currentWishId });
    },
    searchs() {
      return this.$store.state.searchs;
    },
    basket() {
      return this.$store.getters['selection/getOrdreredSelectedWishes'];
    },
    currentWishResults() {
      if (this.currentWish && this.currentWish.name) {
        return this.$store.state.product.searchs[this.currentWish.name];
      }
      return [];
    },
    currentWishIsEmpty() {
      return !this.$store.state.singleton.currentWishId;
    },
    matchedWishesLength() {
      return Object.keys(this.$store.getters['selection/getMatchedWishes']).length;
    },
    basketFull() {
      return this.matchedWishesLength === this.basket.length;
    },
    total() {
      return this.$store.getters['transaction/basketAmount'];
    },
    nextInfos() {
      const length = this.basket.length;
      const successClass = 'btn-outline-success';
      const warningClass = 'btn-outline-warning';
      const classButtonNext = length === this.matchedWishesLength ? successClass : warningClass;
      const textLabelNext = classButtonNext === 'active' ? '' : 'Il reste des produits à ajouter';
      return {
        class: classButtonNext,
        text: textLabelNext,
      };
    },
  },
  mounted() {
    if (this.currentWish) {
      const name = this.currentWish.name;
      this.$store.dispatch('product/fetchSearch', { name });
    } else {
      this.$store.dispatch('currentWish/next');
    }
  },
  methods: {
    // lazyloading
    // lorsqu'on atteint le bas de la page a 100px pret on augmente le nombre de produits affichable
    handleScroll: () => {
      // const footerEl = document.getElementById('footer');
      // const CheckIfScrollBottom = debouncer(function() {
      //   if(getDocHeight() == getScrollXY()[1] + window.innerHeight) {
      //      this.maxProducts += 20;
      //   }
      // },500);
      // if($(window).scrollTop() + $(window).height() > $(document).height() - 100) {
      //    this.maxProducts += 20;
      // }
    },
  },
  components: { CurrentWish, ProductItem },
};
</script>

<style scoped>
.waiting{
  background-color: white;
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
  margin: 5px;
}
.product-img{
  background-color: white;
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
  background-color: white;
  cursor: pointer;
  text-align: center;
  padding: 2px;
  width: 163px;
  margin-left: -6px;
  height: 32px;
  line-height: 32px;
}
.text-atb{
  line-height: 32px;
}
.nothing-box{
}
</style>
