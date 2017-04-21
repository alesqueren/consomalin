<template lang="pug">
  div#currentWish(v-if="currentWish")
    div.input-group.stylish-input-group.search-wrapper
      span.input-group-addon.search-gname
        div {{ currentWish.gname }}
      .input-wrapper.search-input(onclick="javascript:document.getElementById('search-text').focus();")
        .input-input
          input#search-text.form-control(type="text" v-model="currentWish.name" v-on:keyup="rename", tabindex="0" autofocus)
        span.search-search
          span.fa.fa-search
      span.input-group-addon.search-uncheck(@click="remove")
        span.fa.fa-check-square-o.checked-box &nbsp;&nbsp;&nbsp;
        span.fa.fa-square-o.not-checked-box &nbsp;&nbsp;&nbsp;&#8239;
        span Décocher
      //- span.input-group-addon.search-addGroup(@click="addGroup")
      //-   span.fa.fa-list-ul &nbsp;&nbsp;&nbsp;
      //-   span Créer une liste de {{ currentWish.name }}
</template>

<script>
import Wish from '../Basket/Wish';
import router from '../../router';

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
    currentWish() {
      const currentWid = this.$store.state.singleton.currentWid;
      return this.$store.getters['wishGroup/getWish']({ wid: currentWid });
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
    //     this.$store.dispatch('wishGroup/removeWish', { wid: this.currentWish.id });
    //     this.$store.dispatch('singleton/unset', { key: 'currentWish.id' });
    //     this.$store.dispatch('singleton/set', {
    //       key: 'activeGroupId',
    //       value: gid,
    //     });
    //     router.push({ name: 'wishlist' });
    //   });
    // },
    remove() {
      const wid = this.currentWish.id;
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
  border: none;
  height: 44px;
  line-height: 44px;
  width: 100%;
}
.input-wrapper{
  width: 100%;
  float:left;
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
.search-input{
  position: relative;
}
.search-gname{
  display: table-cell;
  font-size: 1.5em;
  width: 150px;
}
.search-search{
  position: absolute;
  color: #0275d8;
  right: 10px;
  top: 10px;
  z-index: 10;
  font-size: 1.5em;
}
.search-text{
  display: table-cell;
}
/* DESELECTIONNER */
.search-uncheck{
  position: relative;
  cursor: pointer;
  display: table-cell;
  width: 130px;
  border-right: 1px solid rgba(0,0,0,.15);
}
.search-uncheck:hover{
  background-color: #e6e6e6;
}
.search-uncheck .not-checked-box{
  position: absolute;
  left: 16px;
  top: 13px;
  visibility: hidden;
}
.search-uncheck:hover .checked-box{
  visibility: hidden;
}
.search-uncheck:hover .not-checked-box{
  visibility: inherit;
}
</style>
