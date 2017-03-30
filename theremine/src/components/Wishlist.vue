<template lang='pug'>
  div#wishlist.container-fluid
    router-link(:to='{ name: "section" }')
      button.btn.btn-success.right(type="button") Passer aux rayons
    div.row.no-gutter
      div.row.no-gutter
        WishGroup(v-for="(wishgroup, wishgroupIndex) in wishlist" v-bind:id="wishgroup.id" v-bind:name="wishgroup.name" v-bind:key="wishgroupIndex")
        input(v-model="newGroupName" v-on:keyup.enter="addWishGroup" placeholder="Add a wishGroup")

    div.row.no-gutter
      div.row.no-gutter
        WishGroupItem(v-for="(wishgroup, wishgroupIndex) in wishlist" v-bind:wishgroup="wishgroup" v-bind:key="wishgroupIndex")
    router-link(:to='{ name: "section" }')
      button.btn.btn-success.right(type="button") Passer aux rayons
</template>

<script>
import WishGroupItem from './Wishlist/WishGroupItem';
import WishGroup from './Wishlist/WishGroup';

export default {
  data() {
    return {
      newGroupName: '',
    };
  },
  computed: {
    // mapState(['wishGroups']),
    // wishGroups() {
    //   return this.$store.state.wishGroups;
    // },
    // selectedWishes() {
    //   return this.$store.state.currentBasket.selectedWishes;
    // },
    // ...mapGetters({ wishlist: 'getWishlist' }),
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
  components: { WishGroupItem, WishGroup },
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
</style>
