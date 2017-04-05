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
            wish-item(v-for="wish in basket" v-bind:wish="wish" v-bind:key="wish.id")
            div Total : {{total}} €
            div Produits au panier : {{matchedWishes}}/{{basket.length}}
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
    searchs() {
      return this.$store.state.searchs;
    },
    currentWish() {
      return this.$store.getters.getCurrentWish;
    },
    basket() {
      return this.$store.getters['basket/getBasket'];
    },
    currentWishResults() {
      return this.$store.state.searchs[this.currentWish.name];
    },
    currentWishIsEmpty() {
      return !this.$store.state.basket.currentWishId;
    },
    matchedWishes() {
      let matchedWishes = 0;
      for (let i = 0; i < this.basket.length; i += 1) {
        matchedWishes += this.basket[i].product.id ? 1 : 0;
      }
      return matchedWishes;
    },
    basketFull() {
      return this.basket.length && this.matchedWishes === this.basket.length;
    },
    total() {
      return this.basket.reduce((prev, wish) => {
        if (wish.product && wish.product.infos && wish.product.infos.price) {
          const price = wish.product.infos.price;
          const priceProduct = price ? price * wish.product.quantity : 0;
          return prev + priceProduct;
        }
        return prev;
      }, 0).toFixed(2);
    },
    nextInfos() {
      const length = this.basket.length;
      const successClass = 'btn-outline-success';
      const warningClass = 'btn-outline-warning';
      const classButtonNext = length === this.matchedWishes ? successClass : warningClass;
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
      this.$store.dispatch('searchProductsWithName', { name });
    } else {
      this.$store.dispatch('nextCurrentWish');
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
