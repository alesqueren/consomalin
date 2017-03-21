<template lang='pug'>
  div.wishgroup.list-group-item.col-xs-6
    div
      button.btn.btn-danger.right(@click="removeWishGroup")
        i.fa.fa-trash-o.fa-lg
    div
      span {{ wishgroup.name }}
    div
      wishItem(v-for="(wish, wishIndex) in wishgroup.wishes" v-bind:wishgroups="wishgroups"  v-bind:wishgroup="wishgroup" v-bind:wish="wish" v-bind:wishIndex="wishIndex")
    div
      input(v-model="newText" v-on:keyup.enter="addWish" placeholder="Add a wish")
</template>

<script>
import wishItem from './wishItem'

export default {
  methods: {
    addWish: function () {
      var self = this;
      $.ajax({
        type: 'POST',
        url : '/wishlist/groups/'+self.wishgroup.id+'/wishes/bulk',
        data: { names : [ self.newText ] },
        complete: function(responseObject) {
          self.wishgroup.wishes.push({id : responseObject.responseJSON, name:self.newText, selected:true});
          self.newText = ''
        }
      });
    },

    removeWishGroup: function () {
      var self = this;
      // console.log(' gid : ' + gid)
      $.ajax({
        type: 'DELETE',
        url : '/wishlist/groups/'+self.wishgroupindex,
        data: {},
        complete: function(responseObject) {
          self.wishgroups.splice(self.wishgroupindex, 1);
        }
      });
    }
  },
  components: { wishItem },
};

</script>

<style scoped>
.wishgroup {
  color: #00B7FF;
}
</style>
