<template lang='pug'>
  div#wishlist.container-fluid
    router-link(:to='{ name: "section" }')
      button.btn.btn-success.right(type="button") Passer aux rayons
    h2 Mes listes :
    div.row.no-gutter.groups
      WishGroup(v-for="wishgroup in wishlist" 
        v-bind:wishgroup="wishgroup"
        v-bind:key="wishgroup")
      input(v-model="newGroupName" v-on:keyup.enter="addWishGroup" placeholder="Add a wishGroup")
    div.col.no-gutter.activeGroup
      ActiveWishGroup()
    h2 Ma liste de course :
    div.row.no-gutter
      div.row.no-gutter
        WishGroupItem(v-for="wishgroup in wishlist" 
          v-bind:wishgroup="wishgroup" 
          v-bind:key="wishgroup")
    router-link(:to='{ name: "section" }')
      button.btn.btn-success.right(type="button") Passer aux rayons
</template>

<script>
import WishGroupItem from './Wishlist/WishGroupItem';
import WishGroup from './Wishlist/WishGroup';
import ActiveWishGroup from './Wishlist/ActiveWishGroup';

export default {
  data() {
    return {
      newGroupName: '',
    };
  },
  computed: {
    wishlist() {
      return this.$store.getters.getWishlist;
    },
  },
  methods: {
    addWishGroup() {
      this.$store.dispatch('addWishGroup', this.newGroupName);
      this.newGroupName = '';
    },
  },
  mounted() {
    this.$store.dispatch('updateWishGroupsAndCurrentBasket');
  },
  components: { WishGroupItem, WishGroup, ActiveWishGroup },
};
</script>

<style scoped>
#wishlist {
  padding: 50px;
  font: 14px "Lucida Grande", Helvetica, Arial, sans-serif;
}
.no-gutter {
  margin-bottom:75px;
}
div.active-group {
  display: inline-block;
}
div.groups {
  margin-bottom: 0px;
}
</style>
