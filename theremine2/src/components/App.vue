<template lang="pug">
  #app
    div#header
      div.container-fluid
        div#navbar.row.no-gutter
          div 
            a(href='/')
              img.logo(src="../assets/images/car.jpg")
              span.brand Consomalin
              div(v-if='user')
                a.title(href="/wishlist") Ma liste de course
                a.title(href="/section") Les rayons
                a.title(href="/basket") Mon panier
                a.title(href="/withdraw") Retrait
          div(v-if='!user')
            router-link.connectRegister(to='/users/loginRegister') Se connecter S'inscrire
          div(v-else)
            span {{user._id}}
            a.btn.btn-info.btn-sm(href='/users/signout')
            span.fa.fa-sign-out
            span Log out
    div#content
      div
        router-link(to="/") Go to home
      div
        router-link(to="/login") Go to login
      div
        router-link(to="/register") Go to register
      div
        router-link(to="/profile") Go to profile
      div
        span counter: {{ count }}
      div
        button(@click="add()") add
      hr
      router-view
    div#footer
      div.footer consomalin 2017
</template>

<script>

import { mapState } from 'vuex';

export default {
  name: 'app',
  computed: mapState([
    'count',
  ]),
  methods: {
    add() {
      this.$store.dispatch('incrementA');
    },
  },
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
