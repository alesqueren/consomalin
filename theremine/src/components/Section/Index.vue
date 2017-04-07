<template lang='pug'>
  div#wishes
    .container-fluid
      .row
        .col-md-10
          CurrentWish
          .container(v-if="!currentWishIsEmpty")
            .row(v-if="currentWishResults")
                ProductItem(
                  v-for="(pid, key, i) in currentWishResults" 
                  v-if="i < maxProducts" 
                  v-bind:maxProducts="maxProducts" 
                  v-bind:pid="pid" 
                  v-bind:key="i")
          .container(v-else-if="basketFull")
            .row
              div
                span Votre liste de course est complète ! Vous pouvez 
                  router-link(:to='{ name: "basket" }')
                    button.btn(v-bind:class="nextInfos.class" type="button") Passer au panier 
                span  pour finaliser la commande.
        .col-md-2
            wish-item(v-for="wid in basket" v-bind:wid="wid" v-bind:key="wid")
            div Total : {{total}} €
            div Produits au panier : {{matchedWishesLength}}/{{basket.length}}
            router-link(:to='{ name: "basket" }')
              button.btn(v-bind:class="nextInfos.class" type="button") Passer au panier

    router-link(:to='{ name: "wishlist" }')
      button.btn.btn-primary.left(type="button") Revenir à la wishlist
</template>

<script>
import WishItem from './WishItem';
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
      const matchedWishes = this.$store.getters.getMatchedWishes;
      return matchedWishes ? matchedWishes.length : 0;
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
  components: { CurrentWish, WishItem, ProductItem },
};
</script>

<style scoped>
</style>
