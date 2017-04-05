<template lang='pug'>
  div#currentWish(v-if="currentWish")
    div.input-group.stylish-input-group.search-wrapper
      span.input-group-addon.search-search
        div
          span.fa.fa-search
      .input-wrapper.search-input(onclick="javascript:document.getElementById('search-text').focus();")
        .input-badge
          span.badge.badge-success {{ currentWish.gname }}
        .input-input
          input#search-text.form-control(type="text" v-model="currentWish.name" v-on:keyup="rename")
      span.input-group-addon.search-addGroup(@click="addGroup")
        span.fa.fa-list-ul &nbsp;&nbsp;&nbsp;
        span Est-ce une liste ?
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
    currentWish() {
      return this.$store.getters.getCurrentWish;
    },
  },
  methods: {
    rename() {
      clearTimeout(this.delayTimer);
      this.delayTimer = setTimeout(() => {
        const name = this.currentWish.name;
        this.$store.dispatch('wishlist/wish/rename', {
          gid: this.currentWish.gid,
          wid: this.currentWish.id,
          name,
        });
        this.$store.dispatch('searchProductsWithName', { name });
      }, 200);
    },
    addGroup() {
      this.wishCreation = true;
      const name = this.currentWish.name;
      this.$store.dispatch('wishlist/group/add', name).then((gid) => {
        this.$store.dispatch('removeCurrentWish');
        this.$store.dispatch('wishlist/wish/remove', { wid: this.currentWish.id });
        this.$store.dispatch('wishlist/group/setActivation', gid);
        router.push({ name: 'wishlist' });
      });
    },
  },
};
</script>

<style scoped>
#currentWish{
  padding-bottom: 25px;
}
#search-text{
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
}
.search-text{
  display: table-cell;
}
.search-addGroup{
  display: table-cell;
}
</style>
