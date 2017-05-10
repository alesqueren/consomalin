<template lang="pug">
  div.root
    Wish(
      v-if="!currentWishHasProduct",
      v-bind:wid="currentWish.id",
      v-bind:badgeLabel="\"Choix en cours\"",
      v-bind:fillerMessage="\"Faites votre selection.\"",
      )
    Wish(
      v-else,
      v-bind:wid="currentWish.id",
      v-bind:badgeLabel="\"Produit choisis\"",
      v-bind:fillerMessage="\"Faites votre selection.\"",
      )
    div(style="clear:both")
    Wish(
      v-for="wid in sameNameWishIds",
      v-bind:wid="wid",
      v-bind:key="wid",
      v-bind:badgeLabel="\"Du même nom\"",
      v-bind:displayGroup="true",
      )
    div(style="clear:both")
    Wish(
      v-if="lastAddedWishId && sameNameWishIds.length === 0",
      v-bind:wid="lastAddedWishId",
      v-bind:badgeLabel="\"Dernier ajout\"",
      )
    div(style="clear:both")
</template>
<script>
import Wish from '../Basket/Wish';

export default {
  props: [],
  data() {
    return {
      delayTimer: null,
    };
  },
  computed: {
    currentWish() {
      return this.$store.getters['sectionWishes/getCurrent'];
    },
    currentWishHasProduct() {
      const gid = this.currentWish.gid;
      const wid = this.currentWish.id;
      let result = null;
      if (this.currentWish) {
        result = Object.keys(this.$store.state.selection.basket[gid][wid]).length;
      }
      return result;
    },
    lastAddedWishId() {
      return this.$store.getters['sectionWishes/getLastAdded'];
    },
    sameNameWishIds() {
      return this.$store.getters['sectionWishes/getSameNameWishIds'];
    },
  },
  components: { Wish },
};
</script>

<style scoped>
.root {
  overflow-y: auto;
  overflow-x: hidden;
  max-height: 80%;
  margin-top: 75px
}
.root > div {
  margin-bottom:10px;
}
.currentWishProducts {
  height: auto;
  min-width: 320px;
  width: 320px;
  padding: 5px;
  background-color: color(--white);
  border: 1px solid grey;
}
.titleCurrent {
  clear: both;
  font-size: 1.5em;
  padding-bottom: 10px;
}
.titlePrevious {
  clear: both;
  font-size: 1.5em;
  padding: 10px 0 10px 0;
}
.grey-btn {
  position: relative;
  width: 150px;
  height: 44px;
  float: right;
  cursor: pointer;
  text-align: center;
}
.grey-btn .special-fa {
  position: absolute;
  right: 10px;
  top: 12px;
}
.grey-btn:hover {
  background-color: #e6e6e6;
}
.basket {
  margin-bottom: 30px;
}
.next {
  margin: 30px 0 30px 0;
  background-color: #5BC0B2;
  border-radius: 3px;
}
.next:hover {
  background-color: #4AB080;
}
</style>
