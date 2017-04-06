<template lang='pug'>
  div
    div.col.activeGroup.col-3(v-if="wishgroup")
      h2 Elements de {{ wishgroup.name }}:
      div.wishgroup.list-group-item.col-3(v-if='wishgroup')
        div
          Wish(v-for="wish in wishgroup.wishes" 
            v-bind:wid="wish.id" 
            v-bind:gid="wishgroup.id" 
            v-bind:key="wish.id")
          input(v-model="newWishName" v-on:keyup.enter="addWish" placeholder="Add a wish" onclick="event.stopPropagation()")

    router-link(:to='{ name: "section" }')
      button.btn.btn-success.right(type="button") Passer aux rayons
</template>

<script>
import Wish from './Wish';

export default {
  props: [],
  data() {
    return {
      newWishName: '',
    };
  },
  computed: {
    wishgroup() {
      const gid = this.$store.state.singleton.activeGroupId;
      if (gid) {
        return this.$store.getters['wishGroup/getGroup'](gid);
      }
      return null;
    },
  },
  methods: {
    addWish() {
      this.$store.dispatch('wishGroup/addWish', {
        gid: this.wishgroup.id,
        name: this.newWishName,
      });
      this.newWishName = '';
    },
  },
  components: { Wish },
};

</script>

<style scoped>
.wishgroup {
  position: relative;
  /*color: #00B7FF;*/
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
