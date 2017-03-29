<template lang='pug'>
  div#wishes
    .container
      .row
        .col-md-10
          currentWish(v-bind:currentwish="currentWish" v-on:new_name="searchProducts(currentWish)")
          .container(v-if="currentWish")
            .row(v-if="currentWishHasMatchingProducts")
                product-item(v-for="(product, productKey, productIndex) in currentWishHasMatchingProducts" v-if="productIndex < maxProducts" v-bind:maxProducts="maxProducts" v-bind:wish="currentWish" v-bind:productkey="productKey" v-bind:product="product" v-on:select_product="bindCurrentWishWithProduct" v-bind:key="productIndex" v-bind:currentWish="currentWish")
          .container(v-else-if="matchedWishes == basket.length")
            .row
              div
                span Votre liste de course est complète ! Vous pouvez 
                  a(href='/basket')
                    button.btn(v-bind:class="nextInfos.class" type="button") Passer au panier 
                span  pour finaliser la commande.
        .col-md-2
            wish-item(v-for="wish in basket" v-bind:wish="wish" v-on:new_current_wish="newCurrentWish" v-on:remove_wish="removeWish" v-bind:key="wish.id")
            div Total : {{total}} €
            div Produits au panier : {{matchedWishes}}/{{basket.length}}
            a(href='/basket')
              button.btn(v-bind:class="nextInfos.class" type="button") Passer au panier

    a(href='/wishlist')
      button.btn.btn-primary.left(type="button") Revenir à la wishlist
</template>

<script>
import WishItem from './Section/WishItem';
import CurrentWish from './Section/CurrentWish';
import ProductItem from './Section/ProductItem';

function getFirstUnmatchedSelectedWish(basket) {
  for (let i = 0; i < basket.length; i++) {
    const wish = basket[i];
    if (!wish.product.id) {
      wish.current = true;
      return { groupid: wish.groupId, wishid: wish.id };
    }
  }
  return null;
}

/*eslint-disable */
function debouncer(a,b,c){var d;return function(){var e=this,f=arguments,g=function(){d=null,c||a.apply(e,f)},h=c&&!d;clearTimeout(d),d=setTimeout(g,b),h&&a.apply(e,f)}}
function getScrollXY(){var a=0,b=0;return"number"==typeof window.pageYOffset?(b=window.pageYOffset,a=window.pageXOffset):document.body&&(document.body.scrollLeft||document.body.scrollTop)?(b=document.body.scrollTop,a=document.body.scrollLeft):document.documentElement&&(document.documentElement.scrollLeft||document.documentElement.scrollTop)&&(b=document.documentElement.scrollTop,a=document.documentElement.scrollLeft),[a,b]}
function getDocHeight(){var a=document;return Math.max(a.body.scrollHeight,a.documentElement.scrollHeight,a.body.offsetHeight,a.documentElement.offsetHeight,a.body.clientHeight,a.documentElement.clientHeight)}
/*eslint-enable */
// function unselectWish(wish) {
  // var self = this;
  // var gid = wish.groupId;
  // $.ajax({
  //   type: 'PUT',
  //   url : '/wishlist/groups/'+gid+'/wishes/'+wish.id,
  //   data: { selected: false},
  //   complete: function(responseObject) {
  //   }
  // });
// }

export default {
  data() {
    return {
      // wishGroups: wishGroups,
      // pSelectedWishes: pSelectedWishes,
      // currentWish: currentWish,
      // maxProducts: 40,
      maxProducts: 40,
    };
  },
  mounted() {
    this.$store
      .dispatch('updateWishGroupsAndCurrentBasket')
      .then(() => {
        this.$store
          .dispatch('processCurrentWish');
      });
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
    currentBasket() {
      return this.$store.state.currentBasket;
    },
    currentWishHasMatchingProducts() {
      return this.currentWish && this.searchs[this.currentWish.name];
    },
    currentWish() {
      // if (this.$store.state.currentBasket) {
      return this.$store.getters.getCurrentWish;
      // }
      // return null;
    },
    basket() {
      const basket = this.$store.getters.getBasket;
      return basket;
    },
    matchedWishes() {
      let matchedWishes = 0;
      for (let i = 0; i < this.basket.length; i += 1) {
        matchedWishes += this.basket[i].product.id ? 1 : 0;
      }
      return matchedWishes;
    },
    total() {
      return this.basket.reduce((prev, wish) => {
        if (wish.product && wish.product.price) {
          const price = wish.product.price;
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
  methods: {
    sendCurrentWish: () => {
    // sendCurrentWish: (wish) => {
      // $.ajax({
      //   type: 'PUT',
      //   url : '/wishlist/groups/'+wish.groupId+'/wishes/'+wish.id+'/current',
      //   data: {},
      //   complete: function(responseObject) {
      //   }
      // });
    },

    searchProducts: () => {
    // searchProducts: (wish) => {
      // const self = this;
      // $.ajax({
      //   type: 'GET',
      //   url : '/products/search/'+wish.name,
      //   data: {},
      //   complete: function(responseObject) {
      //     var products = JSON.parse(responseObject.responseText);
          // test de sort
          // function compare(a,b) {
          //   if (a.price < b.price)
          //     return -1;
          //   if (a.price > b.price)
          //     return 1;
          //   return 0;
          // }
          // var sortedKeyProducts = Object.keys(products).sort( function(keyA, keyB) {
          //     return products[keyA].price - products[keyB].price;
          // });
          // var sortedProducts = [];
          // for(vari=0;i<sortedKeyProducts.length;i++){
          //     sortedProducts[sortedKeyProducts[i]] = products[sortedKeyProducts[i]];
          // }
          // console.log(sortedProducts)
          // self.currentWish.matchingProducts = sortedProducts;
      //     self.currentWish.matchingProducts = products;
      //   }
      // });
    },

    newCurrentWish: (wish) => {
      const self = this;
      this.maxProducts = 40;
      // on notifie le serveur du nouveau wish courrant
      if (this.basket.currentWish !== wish) {
        this.basket.currentWish = wish;
        this.sendCurrentWish(this.basket.currentWish);
        // si on a aucun produit pour ce wish
        if (!Array.isArray(wish.matchingProducts) || wish.matchingProducts.length < 1) {
          // on les recherche
          this.searchProducts(this.basket.currentWish);
        } else {
          self.currentWish.basket.matchingProducts = self.basket.currentWish.matchingProducts;
          // console.log(wish.name + ' already have products');
          // console.log(wish.matchingProducts)
        }
      } else {
        // toujours le meme wish (reclick ou dernier de la liste)
      }
      for (let i = 0; i < this.selectedWishes.length; i += 1) {
        this.selectedWishes[i].current = false;
        if (this.selectedWishes[i] === this.currentWish) {
          this.selectedWishes[i].current = true;
        }
      }
    },

    removeWish: () => {
    // removeWish: (pWish) => {
      // console.log('here')
      // if ( pWish.current ) {
      //   this.pSelectedWishes[pWish.groupId][pWish.id] = false;
      //   this.removeCurrentWish();
      //   this.setCurrentWishToNext();
      // }
      // var psw = this.selectedWishes;
      // for(var i = 0; i < psw.length; i++ ) {
      //   var wish = psw[i];
      //   if ( wish.id == pWish.id) {
      //     this.pSelectedWishes[wish.groupId][wish.id] = false;
      //   }
      // }
      // unselectWish(pWish);
    },

    removeCurrentWish: () => {
      if (this.basket.currentWish) {
        for (let i = 0; i < this.selectedWishes.length; i += 1) {
          this.selectedWishes[i].current = false;
        }
        this.basket.currentWish = null;
        // $.ajax({
        //   type: 'PUT',
        //   url : '/wishlist/removeCurrent',
        //   data: {},
        //   complete: function(responseObject) {

        //   }
        // });
      }
    },

    setCurrentWishToNext: () => {
      // si il reste un wish non lié a un produit, on passe a celui ci
      // sinon on supprime le currentwish et on envoi la requet de remove au serveur
      const nextCurrentWish = getFirstUnmatchedSelectedWish(this.basket);
      if (nextCurrentWish) {
        this.newCurrentWish(nextCurrentWish);
      } else {
        this.removeCurrentWish();
        // console.log(this.selectedWishes[currentIndex].name + ' est le dernier wish')
      }
    },

    // on attache le produit (selectionné) au wish en cours
    bindCurrentWishWithProduct: () => {
    // bindCurrentWishWithProduct: (key, product) => {
      // console.log('cou')
      // var self = this;
      // if ( self.currentWish ) {
      //   this.currentWish.product.id = key;
      //   this.currentWish.product.infos = product;
      //   this.currentWish.product.quantity = 1;
      //   const groupId = this.currentWish.groupId;
      //   const wishId = this.currentWish.id;
      //   $.ajax({
      //     type: 'POST',
      //     url : '/wishlist/groups/'+groupId+'/wishes/'+wishId+'/product',
      //     data: {'pid' : key },
      //     complete: function(responseObject) {
      //       var products = JSON.parse(responseObject.responseText);
      //       self.currentWish.matchingProducts = products;
      //       self.maxProducts++;
      //       self.setCurrentWishToNext();
      //     }
      //   });
      // }
    },

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
