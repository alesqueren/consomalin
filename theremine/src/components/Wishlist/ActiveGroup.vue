<template lang="pug">
  div
    div.activeGroup(v-if="wishgroup")
      div.notepad
        .redLine
        h2.title {{ wishgroup.name }}
        Wish(v-for="wish in wishgroup.wishes" 
          v-bind:wid="wish.id" 
          v-bind:gid="wishgroup.id" 
          v-bind:key="wish.id")
        input#newWish(v-model="newWishName",
        placeholder="Ajouter un produit",
        required="required",
        v-on:keyup.enter="addWish",
        @click.stop.prevent="",
        onclick="event.stopPropagation()",
        tabindex="2")
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
        return this.$store.getters['wishGroup/getGroup']({ gid });
      }
      return null;
    },
  },
  methods: {
    addWish() {
      if (this.newWishName !== '') {
        this.$store.dispatch('wishGroup/addWish', {
          gid: this.wishgroup.id,
          name: this.newWishName,
        });
        this.newWishName = '';
      }
    },
  },
  components: { Wish },
};

</script>

<style scoped>
.notepad .title {
  text-transform: capitalize;
}
</style>
