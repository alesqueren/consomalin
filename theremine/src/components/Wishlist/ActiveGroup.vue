<template lang='pug'>
  div
    div.activeGroup(v-if="wishgroup")
      div#wishes
        .lines
        h2.title {{ wishgroup.name }}
        Wish(v-for="wish in wishgroup.wishes" 
          v-bind:wid="wish.id" 
          v-bind:gid="wishgroup.id" 
          v-bind:key="wish.id")
        input#newWish(v-model="newWishName" v-on:keyup.enter="addWish" placeholder="Ajouter un produit" onclick="event.stopPropagation()")

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
        return this.$store.getters['wishGroup/getGroup']({ gid });
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
#wishes {
  position: relative;
  color: #555;
  font-size: 1.5em;
  padding: 0 !important;
  min-width: 250px;
  font-family: courier, monospace;
  border: 1px solid #dedede;
  background-color: white;
}
#wishes .title {
  font-family: gunny;
  font-size: 2em;
  text-decoration: underline;
  text-align: center;
  margin-left: 65px;
}
.activegroupName {
  text-transform: capitalize;
}
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
#newWish{
  width: 100%;
  font-family: gunny;
  border: none;
  font-size: 1.5em;
  height: 50px;
  line-height: 50px;
  padding-left: 92px;
}
</style>
