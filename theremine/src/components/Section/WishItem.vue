<template lang='pug'>
.wish.list-group-item(v-bind:class='{active:wishIsCurrent}', style='padding:5px', @click='setCurrentWish($event)')
  span.fa.fa-remove(@click='removeWish($event)')
  div
    span(style='font-weight:bold') {{wish.groupName}}
    |  {{wish.name}}
  div(v-if='wish.product.infos')
    div  {{wish.product.infos.name}}
    div  {{wish.product.infos.price}}
    |  
    img.col-md-6(style='width:50px;', v-bind:src='wish.product.infos.imageUrl')
    |  
    input.col-md-6(type='number', v-model.number='wish.product.quantity', step='1', value='0', min='1', max='64', v-on:change='changeQty')

</template>

<script>
export default {
  props: ['wish'],
  computed: {
    wishIsCurrent() {
      return this.wish.id === this.$store.getters.getCurrentWish.id;
    },
    wishHasProduct() {
      return this.wish.id === this.$store.getters.getCurrentWish.id;
    },
  },
  methods: {
    removeWish: () => {
      // this.$emit('remove_wish', this.wish);
    },
    setCurrentWish: () => {
      // if (event) {
      //   // console.log(event.target.tagName)
      //   var currentClass = event.target.tagName;
      //   if ( event.target.tagName != 'SPAN' ) {
      //     this.$emit('new_current_wish', this.wish);
      //   }
      // }
    },
    changeQty: () => {
      // $.ajax({
      //   type: 'PUT',
      //   url : '/wishlist/groups/'+this.wish.gid+'/wishes/'+this.wish.id+'/product',
      //   data: {'qty' : this.wish.product.quantity },
      //   complete: function(responseObject) {
      //   }
      // });
    },
  },
};
</script>

<style scoped>
</style>
