<template lang='pug'>
  div.wishgroup.list-group-item.col-xs-6
    div
      button.btn.btn-danger.right(@click="removeWishGroup")
        i.fa.fa-trash-o.fa-lg
    div
      span {{ wishgroup.name }}
    div
      wishItem(v-for="(wish, wishIndex) in wishgroup.wishes" v-bind:wish="wish" v-bind:wishIndex="wishIndex" v-bind:key="wishIndex")
    div
      input(v-model="newWishName" v-on:keyup.enter="addWish" placeholder="Add a wish")
</template>

<script>
import wishItem from './WishItem';

export default {
  props: ['wishlist', 'wishgroup', 'wishgroupindex'],
  data() {
    return {
      newWishName: '',
    };
  },
  methods: {
    addWish() {
      this.$store.dispatch('addWish', {
        group: this.wishgroup,
        name: this.newWishName,
      });
      this.newWishName = '';
    },

    removeWishGroup() {
      this.$store.dispatch('removeWishGroup', this.wishgroup.id);
    },
  },
  components: { wishItem },
};

</script>

<style scoped>
.wishgroup {
  color: #00B7FF;
}
</style>
