<template lang="pug">
  div#homepage
    div#title
      div#banner-title Retrouvez le gout des courses !
      div.desciption Consomalin est un site de courses en ligne independant qui permet de commander chez Auchan drive sur une interface simple, rapide et sans publicité.
      button.go-to-next
        i.fa.fa-angle-double-down.faa-bounce.animated
    div#rassurance
      div.about
        div Pourquoi Consomalin
        div.why Grâce à votre liste de courses, le site vous propose seulement les produits dont vous avez besoin. Fini les sites lents et remplis de pub. Faites vos courses en un eclair sur Consomalin !
        div.about-border
      div.section-item
        div.item
          i.icon.fa.fa-clock-o
          div &nbsp;Rapide
          div.why Reduisez votre temps de courses par 4 !
        div.item
          i.icon.fa.fa-check-circle-o
          div &nbsp;Maitrise du panier
          div.why N'oubliez plus de produit grâce à votre liste
        div.item
          i.icon.fa.fa-bullhorn
          div &nbsp;Sans pub !
      button.go-to-next.blue-text
        i.fa.fa-angle-double-down.faa-bounce.animated
    div#presentation
      div.item
        div.num
          span 1
        div.img
          img.logo(src="../assets/images/liste.png")
        div.txt Faites votre liste de courses comme à la maison
      div.item
        div.num
          span 2
        div.img
          img.logo(src="../assets/images/basket.png")
        div.txt Le site vous popose les produits de votre liste
      div.item
        div.num
          span 3
        div.img
          img.logo(src="../assets/images/withdraw.png")
        div.txt Réglez vos achats chez Auchan lors du retrait
    div#registration(v-if="!user")
      router-link(:to='{ name: "register" }')
        button.btn.btn-primary.connexion(type="button") Inscription
    div#wishlist(v-else)
      router-link(:to='{ name: "wishlist" }')
        span.input-group-addon.nav-btn.prefered
          span Voir ma liste
</template>

<script>
import { mapState } from 'vuex';

export default {
  computed: mapState({
    user: state => state.user.username,
  }),
};
</script>

<style scoped>
@import url('https://fonts.googleapis.com/css?family=Raleway');
/*#content{
  background-image: url('../assets/images/fly.jpg');
  background-repeat: no-repeat;
  background-size: cover;
  background-position: center top;
  background-attachment: fixed;
}*/


#homepage{
  font-weight: lighter;
}
#title{
  position: relative;
  text-align: center;
  background: url('../assets/images/brimstone-blue.jpg') center top no-repeat;
  height: 100vh !important;
  border-bottom: 2px solid var(--color2);
}
#banner-title{
  color: #f5f7fa;
  font-family: 'Raleway';
  font-size: 65px;
  font-weight: 900;
  text-transform: uppercase;
  padding-top: 20%;
}
.desciption{
  color: #a3b1bf;
  font-family: 'Raleway';
  font-size: 24px;
  line-height: 30px;
  padding: 20px;
}
.go-to-next{
  position: absolute;
  bottom: 90px;
  left: 50%;
  background: transparent;
  border: none;
  box-shadow: 0;
  color: #fff;
  font-size: 25px;
}
.faa-bounce.animated {
    -webkit-animation-name: bounce;
    -moz-animation-name: bounce;
    -o-animation-name: bounce;
    animation-name: bounce;
}
.faa-bounce.animated{
  animation: bounce 2s ease infinite;
}

/*LES PLUS*/
#rassurance{
  position: relative;
  clear: both;
  text-align: center;
  font-size: 2.5em;
  padding: 45px;
  height: 100vh
}
#rassurance .about{
  padding: 45px;
}
#rassurance .about-border {
  content: '';
  background: var(--color2);
  height: 2px;
  width: 140px;
  margin-left: 45%;
  margin-top: 45px;
}
#rassurance .item{
  width: 33%;
  float: left;
  border-radius: 5px;
}
.section-item{
  padding: 45px;
}
.icon{
  color: var(--color2);
}
.why{
  font-size: 0.6em;
}
.blue-text{
  color: var(--color2);
}

/*PRESENTATION*/
#presentation{
  padding-top: 50px;
  clear: both;
  text-align: center;
  font-size: 2em;
}
#presentation .item{
  display: table;
  padding: 15px 0 15px 0;
  border-radius: 5px;
}
#presentation .item:nth-child(odd){
  color: var(--color4);
  background-color: var(--color2);
}
#presentation .item>div{
  display: table-cell;
  vertical-align: middle;
  height: 400px;
  width: 33%;
}
#presentation .item .num{
  width: 150px;
  min-width: 150px;
}
#presentation .item .num{
}
#presentation .item:nth-child(odd) .num{
  color: var(--color4);
  background-color: var(--color2);
}
#presentation .item:nth-child(even) .num{
  color: var(--color2);
  background-color: var(--color4);
}
#presentation .item .num span{
  display: block;
  border: 5px solid var(--color4);
  border-radius: 100%;
  width: 70px;
  height: 70px;
  margin: 40%;
  font-size: 2em;
}
#presentation .item:nth-child(even) .num span{
  border: 5px solid var(--color2);
}
#presentation .item .img{
  min-width: 700px;
  min-height: 245.45px;
  background-color: var(--color4);
  border-radius: 5px;
}
#presentation .item .img img{
  max-width: 700px;
}
#presentation .item .txt{
  text-align: center;
  padding-left: 25px;
  font-size: 1em;
}
#registration{
  position: relative;
  text-align: center;
  padding-bottom: 50px;
  top: 33px;
}
#registration a{
  display: block;
  left: 50%;
  position: absolute;
  margin-left: -50px;
}
#wishlist{
  position: relative;
  text-align: center;
  padding-bottom: 50px;
  top: 33px;
}
#wishlist a{
  display: block;
  left: 50%;
  position: absolute;
  margin-left: -50px;
}
@-webkit-keyframes bounce {
    0%, 20%, 50%, 80%, 100% {-webkit-transform: translateY(0);}
    40% {-webkit-transform: translateY(-30px);}
    60% {-webkit-transform: translateY(-15px);}
}

@-moz-keyframes bounce {
    0%, 20%, 50%, 80%, 100% {-moz-transform: translateY(0);}
    40% {-moz-transform: translateY(-30px);}
    60% {-moz-transform: translateY(-15px);}
}

@-o-keyframes bounce {
    0%, 20%, 50%, 80%, 100% {-o-transform: translateY(0);}
    40% {-o-transform: translateY(-30px);}
    60% {-o-transform: translateY(-15px);}
}
@keyframes bounce {
    0%, 20%, 50%, 80%, 100% {transform: translateY(0);}
    40% {transform: translateY(-30px);}
    60% {transform: translateY(-15px);}
}
</style>
