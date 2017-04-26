<template lang="pug">
  #app(@click='finishAllActions', @keyup.esc="finishAllActions", tabindex="0")
    div#header
      div.left
        router-link.title(:to="{ name: 'home' }")
          img.logo(src="../assets/images/ant.png")
          span.brand &nbsp;&nbsp;&nbsp;Consomalin

      div#steps.left(v-if="user && user.username")
        ul.header-tabs
          li.header-tab
            router-link.title(:to="{ name: 'wishlist' }") Listes
          li.header-tab
            router-link.title(:to="{ name: 'section' }") Rayons
          li.header-tab
            router-link.title(:to="{ name: 'basket' }") Panier
          li.header-tab
            router-link.title(:to="{ name: 'withdraw' }") Retrait
      Usercard
      //- Basketcard
    div#content(v-bind:class="{'marginalize' : navCarRequired}")
      div#replay
      router-view
      NavCard(v-if='navCarRequired')
</template>

<script>
import Basketcard from './Basketcard';
import Usercard from './User/Usercard';
import replay from '../replay';
import NavCard from './NavCard';

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
    routeName() {
      return this.$store.state.route.name;
    },
    navCarRequired() {
      const isWishlist = this.routeName === 'wishlist';
      const isSection = this.routeName === 'section';
      const isBasket = this.routeName === 'basket';
      return this.user.username && (isWishlist || isSection || isBasket);
    },
  },
  methods: {
    finishAllActions() {
      // if (!$(event.target).is('.action')) {
      this.$store.dispatch('singleton/unset', {
        key: 'action',
      });
      // }
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
  components: { Usercard, Basketcard, NavCard },
};
</script>

<style>

:root{
  /*
  --color1: #1b5a7a;
  --color2: #1aa59a;
  --color3: #a6ed8e;
  --color4: #f3ffb9;
  --color1-bh-sl: #8BACBC;
  --color1-bl-sh: #051016;
  */

  --color1: #146C78;
  --color2: #0E91A1;
  --color3: #7DCE94;
  --color4: #EFEDE7;

  --success: #48CE6E;
  --warning: orange;
  --white: white;
}

body{
  background-color: var(--color4);
}

#app {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  outline:none;
}
#content {
  position: relative;
  padding: 59px;
}
.marginalize {
  margin-right: 320px;
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
  background-color: var(--color1);
}

#header .logo {
  height: 60px;
  width: 60px;
  background-color: var(--color4);
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
  color: var(--color4);
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
  color: var(--color1) !important;
  background-color: var(--color4);
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
.btn {
  color: white;
  background-color: var(--color2);
  border-color: var(--color2);
}

</style>
