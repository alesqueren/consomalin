<template lang="pug">
  #app
    div#header
      div.left
        router-link.title(:to="{ name: 'home' }")
          img.logo(src="../assets/images/car.jpg")
          span.brand Consomalin

      div#steps.left
        ul.header-tabs
          li.header-tab
            router-link.title(:to="{ name: 'wishlist' }") Liste
          li.header-tab
            router-link.title(:to="{ name: 'section' }") Rayons
              span.title-desc {{matchedWishesLength}}/{{selectedWishesNb}}
          li.header-tab
            router-link.title(:to="{ name: 'basket' }") Panier
              div.title-desc(style=" position:absolute; bottom: 25px;") {{matchedWishesLength}}/{{selectedWishesNb}}
              div.title-desc(style="font-size: 0.8em; position:absolute; bottom: 0px;")

          li.header-tab
            router-link.title(:to="{ name: 'withdraw' }") Retrait
      Usercard
    div#content
      router-view
</template>

<script>
import Usercard from './Usercard';

export default {
  computed: {
    selectedWishesNb() {
      return this.$store.getters['selection/getOrdreredSelectedWishes'].length;
    },
    matchedWishesLength() {
      return Object.keys(this.$store.getters['selection/getMatchedWishes']).length;
    },
  },
  components: { Usercard },
};
</script>

<style>
@import '../static/plugins/font-awesome-4.7.0/css/font-awesome.min.css';

body{
  background-color: #f2f4f7;
}
#app {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  color: #2c3e60;
}

html {
  width: 100%;
  height: 100%;
  margin: 0;
}
body {
  font: 14px "Lucida Grande", Helvetica, Arial, sans-serif;
  width: 100%;
  height: 100%;
  margin: 0;
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
  min-width: 200px;
  position:relative;
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
