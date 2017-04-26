<template lang="pug">
  div#basket
    h2 Mon panier
    div.groups
      Group(v-for="gid in selectedGroups" 
        v-bind:gid="gid"
        v-bind:key="gid")
</template>

<script>
import Group from './Group';
import List from '../List/Index';

export default {
  computed: {
    selectedGroups() {
      return this.$store.state.wishGroup.map(group => group.id);
    },
    basket() {
      return this.$store.getters['selection/getOrdreredSelectedWishes'];
    },
    selectedWishesNb() {
      return this.$store.getters['selection/getOrdreredSelectedWishes'].length;
    },
    matchedWishesLength() {
      return Object.keys(this.$store.getters['selection/getMatchedWishes']).length;
    },
    total() {
      return this.$store.getters['transaction/basketAmount'];
    },
  },
  components: { Group, List },
};
</script>
<style scoped>
#basket{
  position: relative;
  height: auto;
  font: 14px "Lucida Grande", Helvetica, Arial, sans-serif;
  clear: both;
}
#basket:after {
    content:"";
    clear:both;
    display:block;
}
#basket h2 {
  text-align: center;
}
button{
  cursor: pointer;
}
/*
#recap{
  clear: both;
  background-color: #e5e5e5;
  height: 250px;
}
.groups{
  clear: both;
  background-color: #e5e5e5;
  /*height: 250px;*/
}
#missingProduct{
  position: relative;
  text-align: center;
  margin-top: 50px;
  margin-bottom: 50px;
}
#force-continue{
  margin-top: 25px;
}
#basketFull{
  position: relative;
  text-align: center;
  margin-top: 50px;
  margin-bottom: 50px;
}
#startBasket{
  position: relative;
  text-align: center;
  margin-top: 50px;
  margin-bottom: 50px;
}
#startWishlist{
  position: relative;
  text-align: center;
  margin-top: 50px;
  margin-bottom: 50px;
}
#edit-wishlist{
  position: absolute;
  left: 50px;
  top: 100px;
  z-index: 2;
}

#force-continue{
  position: absolute;
  right: 50px;
  top: 32px;
  z-index: 2;
}
</style>
