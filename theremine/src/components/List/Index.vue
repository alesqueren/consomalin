<template lang="pug">
  div#list
    .hole
    .hole
    .hole
    h2
      router-link.title(:to="{ name: 'wishlist' }") Ma Liste
    #groups.groups
      Group(v-for="gid in selectedGroups" 
        v-bind:gid="gid" 
        v-bind:key="gid")
      .empty(v-if="selectedWishNb === 0") Sélectionnez une rubrique ou un produit pour commencer votre liste
</template>

<script>
import config from '../../../config';
// import $ from 'jquery';
// import 'jquery.scrollbar';
import Group from './Group';

const $ = window.$;

export default {
  props: [],
  data() {
    return {
      newWishName: '',
      demo: Boolean(config.demo === 'true'),
    };
  },
  computed: {
    selectedGroups() {
      const wishGroup = this.$store.state.wishGroup;
      if (wishGroup) {
        return wishGroup.map(group => group.id);
      }
      return [];
    },
    selectedWishNb() {
      return this.$store.getters['selection/getOrderedSelectedWishes'].length;
    },
  },
  mounted() {
    // console.log($('body'));
    // $('#list .bottom').on('click', () => {
    //   $('#groups').animate({ scrollTop: $('#groups').offset().top - 50 }, 'slow');
    //   // window.VuePerfectScrollbar = VuePerfectScrollbar;
    // });
    // setTimeout(() => {
    //   const container = $('#groups');
    //   console.log($('#groups').length);
    //   PerfectScrollbar.initialize(container);
    // $('#groups').scrollbar();
    // }, 1000);
    // $(() => {
    // });
    if (this.demo) {
      $('#list').css('top', '+=50px');
      $('#list > h2').css('top', '+=50px');
      $('#groups').css('top', '+=50px');
      $('#groups').css('height', '82%');
    }
  },
  components: { Group },
};

</script>

<style scoped>
#list{
  position: fixed;
  text-align: left;
  width: 184px;
  left: 0;
  top: 50px;
  font-size: 0.9em;
  background-color: white;
  overflow-y: auto;
  overflow-x: hidden;
  height: 100%;
  border: 1px solid #dedede;
  z-index: 10003;
}
#list .groups{
  border-top: 1px solid red;
  width: 184px;
  overflow-y: auto;
  overflow-x: hidden;
  position: fixed;
  left: 0;
  top: 125px;
  background-color: white;
  /*padding-bottom: 10px;*/
  height: 77%;
  /*max-height: 86vh;*/
  background-size: 100% 21px;
  border: 1px solid #dedede;
}
#list h2{
  text-align: center;
  position: fixed;
  left: 32px;
  top: 92px;
  font-family: learningCurve;
}
.hole{
  float: left;
  border: 1px solid var(--color2);
  border-radius: 15px;
  height: 20px;
  width: 20px;
  margin-left: 30px;
  margin-top: 10px;
  background: linear-gradient(0deg, var(--color2), var(--color2-tr));
}
.bottom{
  position: fixed;
  z-index: 2;
  bottom: 2px;
  left: 75px;
  cursor: pointer;
}
.hidden{
  visibility: hidden;
}
.empty{
  padding: 15px;
  font-size: 15px;
}
</style>
