<template lang='pug'>
  div#wishlist Ma wishlist
    WishGroupItem(v-for="(wishgroup, wishgroupIndex) in wishlist" v-bind:wishlist="wishlist" v-bind:wishgroup="wishgroup" v-bind:wishgroupindex="wishgroupIndex" v-bind:key="wishgroupIndex")
    input(v-model="newGroupName" v-on:keyup.enter="addWishGroup" placeholder="Add a wishGroup")

    a(href='/section')
      button.btn.btn-success.right(type="button") Passer aux rayons
</template>

<script>
import WishGroupItem from './Wishlist/WishGroupItem';

export default {
  data() {
    return {
      newGroupName: '',
    };
  },
  computed: {
    // mapState(['wishGroups']),
    wishGroups() {
      return this.$store.state.wishGroups;
    },
    // ...mapGetters({ wishlist: 'getWishlist' }),
    wishlist() {
      return this.$store.getters.getWishlist;
    },
  },
  // watch: {
  //   wishGroups() {
  //     console.log('watch');
  //     // this.wishlist;
  //   },
  // },
  methods: {
    addWishGroup: () => {
      this.$store.dispatch('addWishGroup', this.newGroupName);
      this.newGroupName = '';
    },
  },
  mounted() {
    this.$store.dispatch('updateWishGroupsAndCurrentBasket');
    // console.log('mount');
    // this.wishlist;
  },
  components: { WishGroupItem },
};
</script>

<style scoped>
#wishlist {
  padding: 50px;
  font: 14px "Lucida Grande", Helvetica, Arial, sans-serif;
}
</style>
