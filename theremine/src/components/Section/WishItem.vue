<template lang='pug'>
.wish.list-group-item(v-bind:class='{active:wishIsCurrent}', @click='setCurrentWish()')
  span.fa.fa-remove.wish-remove(@click.prevent.stop='removeWish($event)')
  div
    span.wishgroupname.badge.badge-success {{wish.gname}}
    span.wishname {{wish.name}}
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
      const currentWishId = this.$store.state.currentBasket.currentWishId;
      return this.wish.id === currentWishId;
    },
  },
  methods: {
    removeWish() {
      const wid = this.wish.id;
      const selected = false;
      this.$store.dispatch('selectWish', { wid, selected }).then(() => {
        if (this.wishIsCurrent) {
          this.$store.dispatch('nextCurrentWish');
        }
      });
    },
    setCurrentWish() {
      const gid = this.wish.gid;
      const wid = this.wish.id;
      const name = this.wish.name;
      this.$store.dispatch('setCurrentWish', { gid, wid });
      this.$store.dispatch('searchProductsWithName', { name });
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
.wish{
  padding: 5px;
  min-height: 85px;
  position: relative;
}
.wishgroupname{
  position: absolute;
  top: 5px;
  left: 5px;
}
.wishname{
  font-weight: bold;
}
.wish-remove{
  position: absolute;
  top: 5px;
  right: 5px;
  color: red;
  z-index: 10;
}
</style>
