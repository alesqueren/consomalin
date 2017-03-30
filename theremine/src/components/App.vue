<template lang="pug">
  #app
    div#header
      div.container-fluid
        div#navbar.row.no-gutter
          div 
            router-link.title(:to="{ name: 'home' }")
              img.logo(src="../assets/images/car.jpg")
              span.brand Consomalin

          router-link.title(:to="{ name: 'wishlist' }") Ma liste de course
          router-link.title(:to="{ name: 'section' }") Les rayons
          router-link.title(:to="{ name: 'basket' }") Mon panier
          router-link.title(:to="{ name: 'withdraw' }") Retrait
          Usercard
    div#content
      router-view
    div#footer
      div.footer consomalin 2017
</template>

<script>

import Usercard from './Usercard';

export default {
  components: { Usercard },
};
</script>

<style>
@import '../static/plugins/font-awesome-4.7.0/css/font-awesome.min.css';
#app {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  color: #2c3e50;
}
body {
  font: 14px "Lucida Grande", Helvetica, Arial, sans-serif;
}

a {
  text-decoration:none;
  color:black;
}
a:hover {
  text-decoration:none;
  color:black;
}

#content{
  padding : 50px;
  padding-bottom: 100px;
}

.left{
  float:left;
}
.right{
  float:right;
}
#header{
  line-height: 50px;
    height:50px;
    width:100%;
    background-color:#8CC739;
}
#header .logo {
  height:50px;
  width:50px;
}
#header .brand {
  font-size:20px;
  font-weight:bold;
}
#header .connectRegister{
    text-align:right;
}
#header .title{
    font-size :  36px;
    font-weight: bold;
    margin-left:15px;
}
#footer{
    height:50px;
    width:100%;
    background-color:#8CC739;
    position: fixed;
  text-align:center;
    bottom: 0;
  line-height:50px;
}

#footer .footer {
  text-align:center;
  font-weight:bold;
  font-size :bold;
}
</style>
