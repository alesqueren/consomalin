<template lang='pug'>
  div#wishlist.container-fluid

    div.row
      div.col.main
        h2 Mes listes :
        div.row.no-gutter
          WishGroup(v-for="wishgroup in wishlist" 
            v-bind:wishgroup="wishgroup"
            v-bind:key="wishgroup")
          input(v-model="newGroupName" v-on:keyup.enter="addWishGroup" placeholder="Add a wishGroup")

        h2 Ma liste de course :
        div.row
          WishGroupItem(v-for="gid in selectedGroups" 
            v-bind:gid="gid" 
            v-bind:key="gid")

      ActiveWishGroup

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
    selectedGroups() {
      return this.$store.getters.getSelectedWishGroups;
    },
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
</style>
