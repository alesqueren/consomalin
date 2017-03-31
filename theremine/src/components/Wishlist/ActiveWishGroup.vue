<template lang='pug'>
  div.wishgroup.list-group-item.col-3(v-if='wishgroup')
    div
      wishItem(v-for="wish in wishgroup.wishes" 
        v-bind:wish="wish" 
        v-bind:gid="wishgroup.id" 
        v-bind:key="wish")
      input(v-model="newWishName" v-on:keyup.enter="addWish" placeholder="Add a wish" onclick="event.stopPropagation()")
</template>

<script>
import wishItem from './WishItem';

export default {
  props: ['wishgroup'],
  data() {
    return {
      newWishName: '',
    };
  },
  computed: {
    wishgroup() {
      return this.$store.getters.getActiveWishGroup;
    },
  },
  methods: {
    addWish() {
      this.$store.dispatch('addWish', {
        gid: this.wishgroup.id,
        name: this.newWishName,
      });
      this.newWishName = '';
    },
  },
  components: { wishItem },
};

</script>

<style scoped>
.wishgroup {
  position: relative;
  color: #00B7FF;
}
.groupName {
  font-size: 1.2em;
  color: black;
}
.topright {
  position: absolute;
  top: 0;
  right: 0;
}
.bottomright {
  position: absolute;
  bottom: 0;
  right: 0;
}
</style>

