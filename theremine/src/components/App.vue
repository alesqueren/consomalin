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
            router-link.title(:to="{ name: 'wishlist' }")
              span.fa.fa-list
              span &nbsp;Ma liste
          li.header-tab
            router-link.title(:to="{ name: 'section' }")
              span.fa.fa-hand-pointer-o
              span &nbsp;Rayons
          li.header-tab
            router-link.title(:to="{ name: 'basket' }")
              span.fa.fa-shopping-cart
              span &nbsp;Mon panier
          li.header-tab
            router-link.title(:to="{ name: 'withdraw' }")
              span.fa.fa.fa-car
              span &nbsp;Retrait
      Usercard
    div#content(v-bind:class="{'marginalize' : navCarRequired}")
      div#replay
      router-view
      NavCard(v-if='navCarRequired')
      List(v-if='navCarRequired')
</template>

<script>
import replay from '../replay';
import NavCard from './NavCard/Index';
import Usercard from './User/Usercard';
import List from './List/Index';

const $ = window.$;

export default {
  computed: {
    user() {
      return this.$store.state.user;
    },
    selectedWishNb() {
      return this.$store.getters['selection/getOrderedSelectedWishes'].length;
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
      this.$store.dispatch('singleton/unset', 'action');
      // }
    },
  },
  created() {
    // this.$store.dispatch('sectionWishes/debug');

    $(document).keydown((e) => {
      if (e.which === 37) {
        replay.previous(this.$store, this.$router);
      } else if (e.which === 39) {
        replay.next(this.$store, this.$router);
      }
    });
  },
  components: { Usercard, NavCard, List },
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

  /* TODO: rm -- */
  --color1: #146C78;
  --color2: #0E91A1;
  --color2-tr: rgba(14, 145, 161, 0.5);
  --color2-br: #14CDE5;
  --color3: #7DCE94;
  --color3-2: #63CE81;
  --color3-3: #4DDD76;
  --color4: #EFEDE7;
  --main-font: rgb(85, 85, 85);

  --success: #48CE6E;
  --active: #7DDBD1;
  --warning: orange;
  --white: white;
  --grey: #c6c6c6;
  --danger: #d9534f;
}

body{
  background-color: var(--color4);
  font-family: helvetica;
  font-space: 0.05em;
}

#app {
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  outline:none;
  color: var(--main-font);
}
#content {
  position: relative;
  padding: 50px 0px 0px 0px;
  height: 94vh;
}
.marginalize {
  margin-left: 150px;
  margin-right: 320px;
}

body {
  font-size: 14px;
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

#header {
   /*plus de place pour le contenu*/
  position: fixed;
  z-index: 10000; 
  line-height: 50px;
  height: 50px;
  width: 100%;
  background-color: var(--color1);
}

#header .logo {
  height: 50px;
  width: 50px;
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
  display: block;
  line-height: 50px;
  font-weight: 700;
  font-size: 20px;
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
#steps .title:not(.router-link-active):hover  {
  color:white !important;
  background-color: rgba(255, 255, 255, 0.14902);
}
#steps .title.router-link-active:hover  {
  cursor: default;
}
.btn {
  color: white;
  background-color: var(--color2);
  border-color: var(--color2);
}

</style>
