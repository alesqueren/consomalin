<template lang='pug'>
  div#wishgroups Ma wishlist
    wishgroupItem(v-for="(wishgroup, wishgroupIndex) in wishgroups" v-bind:wishgroups="wishgroups" v-bind:wishgroup="wishgroup" v-bind:wishgroupindex="wishgroupIndex" )
      input(v-model="newText" v-on:keyup.enter="addWishGroup" placeholder="Add a wishGroup")

    a(href='/section')
      button.btn.btn-success.right(type="button") Passer aux rayons
</template>

<script>
import { mapState } from 'vuex';
import WishGroupItem from './Wishlist/WishGroupItem';

export default {
  // data: () => {
  //   return {
  //     newText: '',
  //   };
  // },
  computed: mapState([
    'wishgroups',
  ]),
  methods: {
    // addWishGroup: function (e) {
    //   var self = this;
    //   $.ajax({
    //     type: 'POST',
    //     url : '/wishlist/groups',
    //     data: { name : self.newText},
    //     complete: function(responseObject) {
    //       // console.log(responseObject);
    //       self.wishgroups.push({
    //           id:responseObject.responseJSON,
    //           name:self.newText,
    //           wishes: []
    //       });
    //       self.newText = '';
    //     },
    //   });
    // },
  },
  mounted() {
    this.$store.dispatch('setWishGroups');
  },
  components: { WishGroupItem },
};
</script>

<style scoped>
#wishgroups {
  padding: 50px;
  font: 14px "Lucida Grande", Helvetica, Arial, sans-serif;
}
</style>
