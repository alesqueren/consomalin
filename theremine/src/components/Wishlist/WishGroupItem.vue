<template lang='pug'>
  div.wishgroup.list-group-item.col-3(v-bind:class="{'bg-info': selected}")
    div
      wishItem(v-for="wish in wishgroup.wishes" v-bind:wish="wish" v-bind:key="wish")
    div
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
    selected() {
      return this.$store.getters.isSelectedWishGroup(this.wishgroup.id);
    },
  },
  methods: {
    addWish() {
      this.$store.dispatch('addWish', {
        group: this.wishgroup,
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
