<template lang="pug">
  div#currentWish(v-if="currentWish")
    span.groupName Liste : {{ currentWish.gname }}
    div.input-group.stylish-input-group.search-wrapper
      span.input-group-addon.search-search
        div
          span.fa.fa-search
      .input-wrapper.search-input(onclick="javascript:document.getElementById('search-text').focus();")
        .input-input
          input#search-text.form-control(type="text" v-model="currentWish.name" v-on:keyup="rename", tabindex="0" autofocus)
      //- span.input-group-addon.search-addGroup(@click="addGroup")
      //-   span.fa.fa-list-ul &nbsp;&nbsp;&nbsp;
      //-   span Créer une liste de {{ currentWish.name }}
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
    // addGroup() {
    //   this.wishCreation = true;
    //   const name = this.currentWish.name;
    //   this.$store.dispatch('wishGroup/addGroup', { name }).then((gid) => {
    //     this.$store.dispatch('wishGroup/removeWish', { wid: this.currentWishId });
    //     this.$store.dispatch('singleton/unset', { key: 'currentWishId' });
    //     this.$store.dispatch('singleton/set', {
    //       key: 'activeGroupId',
    //       value: gid,
    //     });
    //     router.push({ name: 'wishlist' });
    //   });
    // },
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

<style scoped>
#currentWish{
  padding-bottom: 25px;
}
.groupName{
  font-size: 1.5em;
}
#search-text{
  font-family: gunny;
  font-size: 2em;
  font-weight: bolder;
  text-align: center;
  border: none;
  height: 44px;
  line-height: 44px;
  width: 100%;
}
.input-wrapper{
  width: 100%;
  border: 1px solid rgba(0,0,0,.15);
  position: relative;
  display: table;
}
.input-wrapper:hover{
  cursor: text;
}
.input-input{
  display: table-cell;
}
.input-badge{
  display: table-cell;
  top: 24px;
  left: 10px;
  padding-right: 10px;
  padding-left: 10px;
  background-color: white;
}
.search-wrapper{
  display: table;
}
.search-search{
  display: table-cell;
  color: #0275d8;
  font-size: 1.5em;
}
.search-text{
  display: table-cell;
}
/*produit du current wish*/

.wish {
  cursor: pointer;
  position: relative;
  float: left;
  height: auto;
  min-width: 320px;
  width: 320px;
  padding: 5px;
  background-color: white;
  border: 1px solid grey;
}
.wish:hover .product-name{
  text-decoration: underline;
}

.product-infos {
  display: table;
}
.product-left {
  display: table-cell;
  width: 100px;
}
.product-right {
  position: relative;
  display: table-cell;
  vertical-align: middle;
  text-align: center;
  height: 125px;
  /*position: absolute;
  right: 20px;
  bottom: 50px;*/
}
.product-number {
  position: absolute;
  left: 0;
  bottom: 10px;
  width: 100%;
}
.product {
  width: 100%;
  max-width: 100%;
}
.wish-name{
  font-family: gunny;
  font-size: 1.5em;
  font-weight: bold;
}
.wish:hover .wish-name{
  text-decoration: underline
}
.product-name{
  position: absolute;
  top: 15px;
  width: 75%;
  font-family: gunny;
  font-size: 1.5em;
  font-weight: bold;
}
.wish-erase{
  visibility: hidden;
  position: absolute;
  font-size: 1.5em;
  top: 5px;
  right: 5px;
  color: #555;
  z-index: 1;
}
.wish:hover .wish-erase{
  visibility: visible;
}
.wish .wish-erase:hover{
  color: orange;
}
.total{
  vertical-align: bottom;
  float: right;
  display: block;
  height: 27px;
  line-height: 27px;
  margin: 5px 0 5px 0;
  font-size: 1.5em;
  font-weight: bold;
}
.buttons {
  position: absolute;
  top: 5px;
  right: 5px;
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
</style>
