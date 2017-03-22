<template lang='pug'>
div.wish.list-group-item.col-xs-6
    button.btn.btn-danger.right(@click="removeWish")
    i.fa.fa-trash-o.fa-lg
    span {{ wish.name }}
    input(type="checkbox" v-model="wish.selected" v-on:change="selectWish")
</template>

<script>

export default {
  methods: {
    selectWish: () => {
      // var self = this;
      // var gid = this.$parent.wishgroup.id;
      // $.ajax({
      //   type: 'PUT',
      //   url : '/wishlist/groups/'+gid+'/wishes/'+this.wish.id,
      //   data: { selected: self.wish.selected},
      //   complete: function(responseObject) {
      //     self.$parent.wishgroup.wishes[self.wishIndex].selected = self.wish.selected;
      //   }
      // });
    },

    removeWish: () => {
      // var self = this;
      // console.log(' gid : ' + gid)
      // $.ajax({
      //   type: 'DELETE',
      //   url : '/wishlist/groups/'+this.wishgroup.id+'/wishes/'+this.wish.id,
      //   data: {},
      //   complete: function(responseObject) {
      //     for(var i=0;i<self.wishgroups.length;i++){
      //       var wishGroup = self.wishgroups[i];
      //       for(var j=0;j<wishGroup.wishes.length;j++){
      //         var wish = wishGroup.wishes[j];
      //         if( wish.id == self.wish.id ){
      //           self.wishgroups[i].wishes.splice(j, 1);
      //         },
      //       },
      //     },
      //   },
      // });
    },
  },
};
</script>

<style scoped>
.wishgroup {
  color: #00B7FF;
}
</style>
