<template lang="pug">
  #app
    div#header
      div.left
        router-link.title(:to="{ name: 'home' }")
          img.logo(src="/images/car.jpg")
          span.brand Consomalin

      div#steps.left
        ul.header-tabs
          li.header-tab
            router-link.title(:to="{ name: 'wishlist' }") Liste
          li.header-tab
            router-link.title(:to="{ name: 'section' }") Rayons
              span.title-desc {{matchedWishes}}/{{ basket.length }}
          li.header-tab
            router-link(:to="{ name: 'basket' }")
              span.title Panier

          li.header-tab
            router-link.title(:to="{ name: 'withdraw' }") Retrait
      Usercard
    div#content
      router-view
</template>

<script>
import { mapState } from 'vuex';
import Usercard from './Usercard';

export default {
  computed: {
    ...mapState({
      user: state => state.user.user,
    }),
    basket() {
      return this.$store.getters['basket/getBasket'];
    },
    matchedWishes() {
      let matchedWishes = 0;
      for (let i = 0; i < this.basket.length; i += 1) {
        matchedWishes += this.basket[i].product.id ? 1 : 0;
      }
      return matchedWishes;
    },
  },
  components: { Usercard },
};
</script>

<style>
@import '../static/plugins/font-awesome-4.7.0/css/font-awesome.min.css';

#app {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  color: #2c3e60;
  background-color: #f2f4f7;
}

body {
  font: 14px "Lucida Grande", Helvetica, Arial, sans-serif;
}

a {
  text-decoration: none !important;
}

a:hover {
  text-decoration: none;
}

#content{
  padding: 60px;
  padding-bottom: 100px;
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
}

#header .brand {
  font-size: 20px;
  font-weight: bold;
}

#header .connectRegister{
  text-align: right;
}

#header .title {
  line-height: 60px;
  font-weight: 700;
  font-size: 22px;
  font-weight: bold;
  margin-left:15px;
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

</style>
