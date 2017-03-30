<template lang='pug'>
  div(v-if="currentwish.id")
    div.input-group.stylish-input-group.search-wrapper
      span.input-group-addon.search-search
        button(type="submit")
          span.fa.fa-search
      .input-wrapper.search-input(onclick="javascript:document.getElementById('search-text').focus();")
        .input-badge
          span.badge.badge-primary {{ currentwish.gname }}
        .input-input
          input#search-text.form-control(type="text" v-model="currentwish.name" v-on:keyup="rename")
      span.input-group-addon.search-addGroup
        span Est-ce une liste ?
        button(type="submit" @click="addGroup")
          span.fa.fa-list-ul
</template>

<script>
export default {
  props: ['currentwish'],
  data() {
    return {
      delayTimer: null,
      wishCreation: false,
    };
  },
  computed: {
    name: {
      get() {
        // return this.$store.getters.getCurrentWish.name;
      },
      set() {
        // set(name) {
        // this.$store.dispatch('renameWish', {
        //   gid: this.currentwish.gid,
        //   wid: this.currentwish.id,
        //   name,
        // });
      },
    },
  },
  methods: {
    rename() {
      clearTimeout(this.delayTimer);
      this.delayTimer = setTimeout(() => {
        const name = this.currentwish.name;
        this.$store.dispatch('renameWish', {
          gid: this.currentwish.gid,
          wid: this.currentwish.id,
          name,
        });
        this.$store.dispatch('searchProductsWithName', { name });
      }, 200);
    },
    addGroup() {
      this.wishCreation = true;
      const name = this.currentwish.name;
      this.$store.dispatch('addWishGroup', name).then((gid) => {
        this.$store.dispatch('addWish', { gid, name }).then((wid) => {
          this.$store.dispatch('setCurrentWish', { gid, wid });
        });
      });
    },
  },
};
</script>

<style scoped>
#search-text{
  text-align:center;
  border: none;
  height: 64px;
  line-height: 64px;
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
  display:table-cell;
}
.input-badge{
  display:table-cell;
  top: 24px;
  left: 10px;
  margin-right: 12px;
}
.search-wrapper{
  display:table;
}
.search-search{
  display:table-cell;
}
.search-text{
  display:table-cell;
}
.search-addGroup{
  display:table-cell;
}
</style>
