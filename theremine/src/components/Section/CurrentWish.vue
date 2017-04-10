<template lang='pug'>
  div#currentWish(v-if="currentWish")
    div.input-group.stylish-input-group.search-wrapper
      span.input-group-addon.search-search
        div
          span.fa.fa-search
      .input-wrapper.search-input(onclick="javascript:document.getElementById('search-text').focus();")
        .input-badge
          span.badge.badge-success.groupName {{ currentWish.gname }}
        .input-input
          input#search-text.form-control(type="text" v-model="currentWish.name" v-on:keyup="rename")
      span.input-group-addon.search-deleteWish.alert.alert-danger(@click="remove")
        span.fa.fa-eraser &nbsp;&nbsp;&nbsp;
        span Effacer de ma liste
      span.input-group-addon.search-addGroup(@click="addGroup")
        span.fa.fa-list-ul &nbsp;&nbsp;&nbsp;
        span Créer une liste de {{ currentWish.name }}
</template>

<script>
import router from '../../router';

export default {
  props: [],
  data() {
    return {
      delayTimer: null,
      wishCreation: false,
    };
  },
  computed: {
    currentWishId() {
      return this.$store.state.singleton.currentWishId;
    },
    currentWish() {
      return this.$store.getters['wishGroup/getWish']({ wid: this.currentWishId });
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
    addGroup() {
      this.wishCreation = true;
      const name = this.currentWish.name;
      this.$store.dispatch('wishGroup/addGroup', { name }).then((gid) => {
        this.$store.dispatch('wishGroup/removeWish', { wid: this.currentWishId });
        this.$store.dispatch('singleton/unset', { key: 'currentWishId' });
        this.$store.dispatch('singleton/set', {
          key: 'activeGroupId',
          value: gid,
        });
        router.push({ name: 'wishlist' });
      });
    },
    remove() {
      const wid = this.currentWishId;
      const selected = false;
      this.$store.dispatch('selection/selectWish', { wid, selected }).then(() => {
        this.$store.dispatch('currentWish/next');
      });
    },
  },
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
.search-addGroup{
  cursor: pointer;
  display: table-cell;
}
.search-deleteWish{
  cursor: pointer;
  display: table-cell;
  width: 130px;
}
</style>
