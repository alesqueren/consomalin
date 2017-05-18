<template lang="pug">
  div#root(v-if="currentWish")
    div.input-group.stylish-input-group.search-wrapper
      span.input-group-addon.search-gname
        div {{ currentWish.gname }}
      .input-wrapper.search-input(
        v-bind:class="{'disabled': hasProducts, 'tooltip': hasProducts}", 
        @click="focus")
        .input-input
          span.tooltiptext.tooltip-bottom(v-if="hasProducts") Retirez les produits du panier pour modifier la recherche 
          input#search-text.form-control(
            type="text",
            v-bind:disabled="hasProducts",
            v-model="currentWish.name",
            v-on:keyup="rename", 
            tabindex="0", 
            autofocus)
        span.search-search
          span.fa.fa-search
</template>

<script>
import Wish from '../Basket/Wish';

export default {
  props: [],
  data() {
    return {
      delayTimer: null,
    };
  },
  computed: {
    currentWish() {
      return this.$store.getters['sectionWishes/getCurrent'];
    },
    productIds() {
      const basket = this.$store.state.selection.basket;
      const products = basket[this.currentWish.gid][this.currentWish.id];
      return Object.keys(products);
    },
    hasProducts() {
      return this.productIds.length !== 0;
    },
  },
  methods: {
    rename() {
      clearTimeout(this.delayTimer);
      this.delayTimer = setTimeout(() => {
        const name = this.currentWish.name;
        this.$store.dispatch('wishGroup/renameWish', {
          wid: this.currentWish.id,
          name,
        });
        this.$store.dispatch('product/fetchSearch', { name });
      }, 200);
    },
    remove() {
      const wid = this.currentWish.id;
      const selected = false;
      this.$store.dispatch('selection/selectWish', { wid, selected }).then(() => {
        this.$store.dispatch('sectionWishes/next');
      });
    },
    focus() {
      document.getElementById('search-text').focus();
    },
  },
  components: { Wish },
};
</script>

<style scoped>
#root {
  padding-bottom: 15px;
}
.search-gname {
  font-size: 1.5em;
  font-family: learningCurve;
}
#search-text {
  font-family: learningCurve;
  font-size: 1.5em;
  font-weight: bolder;
  border: none;
  height: 35px;
  line-height: 35px;
  width: 100%;
}

.input-wrapper {
  width: 100%;
  float:left;
  border: 1px solid rgba(0,0,0,.15);
  position: relative;
  display: table;
}
.input-wrapper.disabled {
  background-color: #eceeef;
}
.input-wrapper.disabled:hover {
  cursor: no-drop;
}
.input-wrapper:not(.disabled):hover {
  cursor: text;
}

.input-input {
  display: table-cell;
}
.input-badge {
  display: table-cell;
  top: 24px;
  left: 10px;
  padding-right: 10px;
  padding-left: 10px;
  background-color: var(--white);
}
.search-wrapper {
  display: table;
}
.search-input {
  position: relative;
  float: left;
  height: auto;
  min-width: 320px;
  /*width: 320px;*/
  padding: 5px;
  background-color: var(--white);
  border: 1px solid rgba(0,0,0,.15);;
  width: 100%;
}
.wish:hover .product-name {
  text-decoration: underline;
}

.product-infos {
  display: table;
}
.search-gname {
  display: table-cell;
  font-size: 1.5em;
  width: 150px;
}
.search-search {
  position: absolute;
  color: #0275d8;
  right: 10px;
  top: 7px;
  z-index: 9;
  font-size: 1.5em;
}
.search-text {
  display: table-cell;
}
/* DESELECTIONNER */
.search-uncheck {
  position: relative;
  cursor: pointer;
  display: table-cell;
  width: 130px;
  border-right: 1px solid rgba(0,0,0,.15);
}
.search-uncheck:hover {
  background-color: #e6e6e6;
}
.search-uncheck .not-checked-box {
  position: absolute;
  left: 12px;
  top: 14px;
  visibility: hidden;
}
.search-uncheck:hover .checked-box {
  visibility: hidden;
}
.search-uncheck:hover .not-checked-box {
  visibility: inherit;
}
.tooltip .tooltiptext {
  width: 240px;
}
.tooltip-bottom{
  top: 135%;
  left: 0;
  margin-left: -60px;
}
</style>
