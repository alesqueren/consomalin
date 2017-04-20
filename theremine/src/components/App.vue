<template lang="pug">
  #app(@click='finishDeletion', @keyup.esc="finishDeletion", tabindex="0")
    div#header
      div.left
        // router-link.title(:to="{ name: 'replay' }")
        router-link.title(:to="{ name: 'home' }")
          img.logo(src="../assets/images/ant.png")
          span.brand &nbsp;&nbsp;&nbsp;Consomalin

      div#steps.left(v-if="user && user.username")
        ul.header-tabs
          li.header-tab
            router-link.title(:to="{ name: 'wishlist' }") Listes
          li.header-tab
            router-link.title(:to="{ name: 'basket' }") Panier

          li.header-tab
            router-link.title(:to="{ name: 'withdraw' }") Retrait
      Usercard
      Basketcard
    div#content
      router-view
</template>

<script>
import Basketcard from './Basketcard';
import Usercard from './User/Usercard';
import replay from '../replay';

const $ = window.$;

export default {
  computed: {
    user() {
      return this.$store.state.user;
    },
    selectedWishesNb() {
      return this.$store.getters['selection/getOrdreredSelectedWishes'].length;
    },
    matchedWishesLength() {
      return Object.keys(this.$store.getters['selection/getMatchedWishes']).length;
    },
  },
  methods: {
    finishDeletion() {
      this.$store.dispatch('singleton/unset', {
        key: 'actionnedEntity',
      });
    },
  },
  created() {
    $(document).keydown((e) => {
      if (e.which === 37) {
        replay.previous(this.$store, this.$router);
      } else if (e.which === 39) {
        replay.next(this.$store, this.$router);
      }
    });
  },
  components: { Usercard, Basketcard },
};
</script>

<style>

body{
  background-color: #f2f4f7;
}
#app {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  color: #2c3e60;
  outline:none;
}

body {
  font: 14px "Lucida Grande", Helvetica, Arial, sans-serif;
}

a {
  text-decoration: none !important;
  display: block;
}

a:hover {
  text-decoration: none;
}


.left{
  float: left;
}

#steps {
  position: absolute;
  left: 50%;
  margin-left: -300px;
}

.right{
  float: right;
}

#header{
  /* plus de place pour le contenu
  position: fixed;
  z-index: 2; */
line-height: 60px;
height: 60px;
width: 100%;
background-color: #21314d;
}

#header .logo {
  height: 60px;
  width: 60px;
  background-color: #f2f4f7;
}

#header .brand {
  font-size: 20px;
  font-weight: bold;
}

#header .connectRegister{
  text-align: right;
}

#header .title {
  min-width: 200px;
  position:relative;
  line-height: 60px;
  font-weight: 700;
  font-size: 22px;
  font-weight: bold;
  color: #747e8f;
}

#header .title-desc {
  font-size: 12px;
}

.header-tabs {
  display: table;
  padding: 0;
  font-size: 0;
  margin: 0;
}

.header-tab {
  display: table-cell;
  margin: 0;
  padding: 0;
  box-sizing: border-box;
  width: 150px;
  text-align: center;
}

#steps .router-link-active {
  color: #13181a !important;
  background-color: #f2f4f7;
  display: block;
}
#steps .header-tab:hover :not(.router-link-active) {
  color:white !important;
  background-color: rgba(255, 255, 255, 0.14902);
  display: block;
}
#steps .header-tab:hover .router-link-active {
  cursor: default;
}

</style>
