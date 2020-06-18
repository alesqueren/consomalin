<template lang="pug">
  div#basket
    h2 Panier
    .border
    div.groups
      Group(v-for="gid in selectedGroups" 
        v-bind:gid="gid"
        v-bind:key="gid")
</template>

<script>
import Group from './Group';
import List from '../List/Index';
import router from '../../router';

export default {
  computed: {
    selectedGroups() {
      return this.$store.state.wishGroup.map(group => group.id);
    },
    basket() {
      return this.$store.getters['selection/getOrderedSelectedWishes'];
    },
    selectedWishNb() {
      return this.$store.getters['selection/getOrderedSelectedWishes'].length;
    },
    matchedWishesLength() {
      return Object.keys(this.$store.getters['selection/getMatchedWishes']).length;
    },
    total() {
      return this.$store.getters['transactions/basketAmount'];
    },
  },
  // mounted() {
  //   const height = $(window).height();
  //   const halfHeight = height / 2;
  //   const halfElement = 175;
  //   $('#groups .wish').on('click', (e) => {
  //     const wid = $(e.currentTarget).data('wid');
  //     const element = $('#basket [data-wid="' + wid + '"]');
  //     $('html, body').animate({ scrollTop: element.offset().top + 50 }, 500);
  //   });
  // },
  created() {
    if (!this.selectedWishNb) {
      router.push({ name: 'wishlist' });
    }
  },
  components: { Group, List },
};
</script>

<style scoped>
#basket{
  position: relative;
  height: auto;
  font: 14px;
  clear: both;
  padding: 30px 65px 30px 65px;
}
#basket:after {
  content:"";
  clear:both;
  display:block;
}
#basket h2 {
  text-align: center;
}
.border{
  content: '';
  background: var(--color2);
  height: 2px;
  width: 130px;
  position: absolute;
  left: 50%;
  margin-left: -65px;
  margin-top: 5px;
}
button{
  cursor: pointer;
}
#recap{
  clear: both;
  background-color: #e5e5e5;
  height: 250px;
}
.groups{
  clear: both;
  /*background-color: #e5e5e5;*/
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
