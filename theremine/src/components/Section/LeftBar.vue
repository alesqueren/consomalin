<template lang="pug">
  div#LeftBar(v-if="currentWish")
    div.btn.uncheck(@click="remove")
      span.fa.fa-check-square-o.checked-box &nbsp;&nbsp;&nbsp;
      span.fa.fa-square-o.not-checked-box &nbsp;&nbsp;&nbsp;&#8239;
      span Décocher
    router-link(:to='{ name: "basket" }')
      div.btn.search-previous(@click="previous")
        span.fa.fa-arrow-left &nbsp;&nbsp;&nbsp;
        span Retourner à ma liste de course
    div(v-if="productIds.length > 0")
      //- div(v-if="productIds.length === 1") Produit actuel
      //- div(v-if="productIds.length > 1") Produits actuels
      Wish(v-bind:wid="currentWish.id" v-bind:gid="currentWish.gid")
      div(style="clear:both")
    div.btn.search-next(@click="next")
      span Produit suivant&nbsp;&nbsp;&nbsp;
      span.fa.fa-arrow-right
</template>
<script>
import router from '../../router';
import Wish from '../Basket/Wish';

export default {
  props: [],
  data() {
    return {
      delayTimer: null,
    };
  },
  computed: {
    multiSelection() {
      return this.$store.state.singleton && this.$store.state.singleton.multiSelection;
    },
    currentWishId() {
      return this.$store.state.singleton.currentWishId;
    },
    currentWish() {
      return this.$store.getters['wishGroup/getWish']({ wid: this.currentWishId });
    },
    productIds() {
      return Object.keys(this.$store.state.selection[this.currentWish.gid][this.currentWish.id]);
    },
  },
  methods: {
    next() {
      if (this.$store.dispatch('currentWish/next', this.currentWishId)) {
        this.finish();
      }
    },
    finish() {
      router.push({ name: 'basket' });
    },
    previous() {
      this.$store.dispatch('currentWish/next', this.currentWishId);
    },
    remove() {
      const wid = this.currentWishId;
      const selected = false;
      this.$store.dispatch('selection/selectWish', { wid, selected }).then(() => {
        this.$store.dispatch('currentWish/next');
      });
    },
  },
  components: { Wish },
};
</script>

<style>
#LeftBar .btn {
  width: 100%;
  cursor: pointer;
}
.search-previous{
  cursor: pointer;
}
.search-previous:hover{
  background-color: #5cb85c;
  color: white;
}
.search-previous{
  text-align: left
}
.search-next{
  text-align: right
}
.search-next:hover{
  background-color: #5cb85c;
  color: white;
}
.uncheck{
  cursor: pointer;
}
.uncheck .not-checked-box{
  display: none;
}
.uncheck:hover .checked-box{
  display: none;
}
.uncheck:hover .not-checked-box{
  display: inherit;
}
</style>
