<template lang="pug">
  #app(@click='finishAllActions', @keyup.esc="finishAllActions", tabindex="0")
    div#header
      div#demo(v-if="demo") Site de démonstration, aucune commande n'est envoyée aux magasins. Aidez-nous en remplissant ce 
        a(href="https://goo.gl/forms/fso29Uz3ItSfkNAl1" target="blank") questionnaire
        span .
      div.left
        router-link.title(:to="{ name: main }")
          div.live(v-if="demo") Live demo
          img.logo(
          v-bind:class="{'inlive': demo}"
          src="../assets/images/logo.png")

      div#steps.left(v-if="user && user.username")
        ul.header-tabs
          li.header-tab
            router-link.title(:to="{ name: 'wishlist' }")
              span.fa.fa-edit
              span &nbsp;Bloc-note
          li.header-tab(v-bind:class="{'inactive': selectedWishNb === 0}")
            router-link.title(:to="{ name: 'section' }")
              span.fa.fa-hand-pointer-o
              span &nbsp;Rayons
          li.header-tab(v-bind:class="{'inactive': selectedWishNb === 0}")
            router-link.title(:to="{ name: 'basket' }")
              span.fa.fa-shopping-cart
              span &nbsp;Panier
          li.header-tab(v-bind:class="{'inactive': matchedWishesLength === 0}")
            router-link.title(:to="{ name: 'withdraw' }")
              span.fa.fa.fa-calendar
              span &nbsp;Retrait
      Usercard
      li.header-tab.right.help
        router-link.title.help(:to="{ name: 'help' }")
          // span.fa.fa.fa-question-circle
          span &nbsp;Aide
    div#content(v-bind:class="{'marginalize-left' : listRequired, 'marginalize-right' : navCarRequired}")
      div#replay
      router-view
      NavCard(v-if='navCarRequired')
      List(v-if='listRequired')
</template>

<script>
import NavCard from './NavCard/Index';
import Usercard from './User/Usercard';
import List from './List/Index';
import config from '../../config';
import replay from '../replay';

const $ = window.$;

export default {
  data() {
    return {
      demo: config.demo,
    };
  },
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
      const isWithdraw = this.routeName === 'withdraw';
      const isTicket = this.routeName === 'ticket';
      const isHelp = this.routeName === 'help';
      return isWishlist || isSection || isBasket || isWithdraw || isTicket || isHelp;
    },
    listRequired() {
      const isWishlist = this.routeName === 'wishlist';
      const isSection = this.routeName === 'section';
      const isBasket = this.routeName === 'basket';
      return this.user.username && (isWishlist || isSection || isBasket);
    },
    main() {
      return this.user.username ? 'wishlist' : 'home';
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
  mounted() {
    if (this.demo) {
      $('#content').css('margin-top', '+=50px');
      $('#contentNavcard').css('top', '+=50px');
      $('#header').css('top', '+=50px');
      // $('#contentList').css('top', '+=50px');
    }
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
  --inactive: #999;
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
  margin-top: 50px;
}
#content {
  position: relative;
  margin-top: 50px;
}
.marginalize-left {
  margin-left: 150px;
}
.marginalize-right {
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
  margin-left: 184px;
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
  min-width: 1200px;
  top: 0;
}

#header .logo {
  height: 43px;
  width: 83px;
  margin-left: 50px;
}
#header .inlive {
  margin-left: 35px;
}
#header .live{
  color: red;
  position: absolute;
  font-size: 12px;
  bottom: -16px;
  right: 22px;
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

#header .inactive .title {
  color: var(--inactive);
}

#header .inactive a {
  cursor: default;
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
  width: 200px;
  text-align: center;
}

.header-tab .router-link-active {
  color: white !important;
  background-color: var( --color2);
  display: block;
  text-shadow: 0 0 0 #000;
}
input[type=number]::-webkit-outer-spin-button,
input[type=number]::-webkit-inner-spin-button {
    -webkit-appearance: none;
    margin: 0;
}
input[type=number] {
    -moz-appearance:textfield;
}
.header-tab:not(.inactive) .title:not(.router-link-active):hover  {
  color:white !important;
  background-color: rgba(255, 255, 255, 0.14902);
}
.title.router-link-active:hover  {
  cursor: default;
}
.btn {
  color: white;
  background-color: var(--color2);
  border-color: var(--color2);
}
.help{
  min-width: 120px !important;
  max-width: 120px !important;
}
#demo{
  position: fixed;
  top: 0;
  right: 0;
  background-color: #d9534f;
  height: 50px;
  width: 100%;
  text-align: center;
  font-size: 18px;
  line-height: 50px;
  color: white;
  border-top-left-radius: 5px;
  border-top-right-radius: 5px;
  z-index: 10001;
}
#demo a{
  display: inline;
  color: var(--color1);
}
</style>
