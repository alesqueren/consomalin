<template lang="pug">
  div#homepage
    div#title
      div#banner-title Retrouvez le gout des courses
      div.desciption {{ presText }}
      button.go-to-next
        i.fa.fa-angle-double-down.faa-bounce.animated
      div#registration(v-if="!user")
        router-link(:to='{ name: "register" }')
          span.input-group-addon.nav-btn.prefered
            span Commencer tout de suite
      div#wishlist(v-else)
        router-link(:to='{ name: "wishlist" }')
          span.input-group-addon.nav-btn.prefered
            span Voir ma liste
      div.huit(v-if='demo')
        .text 8€ offerts pour 50€ d'achat
    div#rassurance
      div.about
        div Pourquoi Consomalin
        div.why Grâce à votre liste de courses, le site vous propose seulement les produits dont vous avez besoin. Fini les sites lents et remplis de pub.
        div.about-border
      div.section-item
        div.item
          i.icon.fa.fa-edit
          div &nbsp;Liste de courses
          div.why Achetez seulement ce dont vous avez besoin.
        div.item
          i.icon.fa.fa-simplybuilt
          div &nbsp;Simple
          div.why Fini les sites qui vous donnent des boutons.
        div.item
          i.icon.fa.fa-check-circle-o
          div &nbsp;Maitrise du panier
          div.why N'oubliez plus de produit grâce à votre liste.
        div.item
          i.icon.fa.fa-clock-o
          div &nbsp;Gagnez du temps
          div.why Faites vos courses 4 fois plus vite.
        div.item
          i.icon.fa.fa-bullhorn
          div &nbsp;Sans pub !
          div.why Le plaisir de naviguer à l'état pur.
        div.item
          i.icon.fa.fa-user-o
          div &nbsp;Independant
          div.why Nous ne sommes pas sponsorisés par les marques ou les distributeurs.
      button.go-to-next.blue-text
        i.fa.fa-angle-double-down.faa-bounce.animated
    div#presentation
      div.about
        div Comment Consommer Malin
        div.about-border
      div.item
        div.num
          span 1
        div.img
          img.logo(src="../assets/images/myList.png")
        div.txt Préparez votre liste de courses comme à la maison
      div.item
        div.num
          span 2
        div.img
          img.logo(src="../assets/images/section.png")
        div.txt Le site vous popose les<br/>produits de votre liste
      div.item
        div.num
          span 3
        div.img
          img.logo(src="../assets/images/retrait.png")
        div.txt Valider votre ticket de caisse et<br/>choisissez un horaire de retrait
      div.item
        div.num
          span 4
        div.img
          img.logo(src="../assets/images/withdraw.png")
        div.txt {{ secondText }} 
    div#registration(v-if="!user")
      router-link(:to='{ name: "register" }')
        button.btn.btn-primary.connexion(type="button") Inscription
    div#wishlist(v-else)
      router-link(:to='{ name: "wishlist" }')
        span.input-group-addon.nav-btn.prefered
          span Voir ma liste
</template>

<script>
import config from '../../config';

const $ = window.$;

export default {
  computed: {
    user() {
      return this.$store.state.user.username;
    },
    demo() {
      return config.MODE_DEMO;
    },
    presText() {
      let text = 'Consomalin est un site de courses en ligne independant qui permet de commander chez Auchan drive Balma.';
      if (this.demo) {
        text = 'Consomalin est un site de courses en ligne independant dans votre drive préféré.';
      }
      return text;
    },
    secondText() {
      let text = 'Réglez vos achats aux bornes de retrait Auchan.';
      if (this.demo) {
        text = 'Réglez vos achats aux bornes de retrait.';
      }
      return text;
    },
  },
  mounted() {
    $('#title .go-to-next').on('click', () => {
      const page = $('#rassurance');
      const speed = 750;
      $('html, body').animate({ scrollTop: $(page).offset().top }, speed); // Go
      return false;
    });
    $('#rassurance .go-to-next').on('click', () => {
      const page = $('#presentation');
      const speed = 750;
      $('html, body').animate({ scrollTop: $(page).offset().top }, speed); // Go
      return false;
    });
  },
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
  background: url('../assets/images/brimstone-blue2.jpg') center top no-repeat;
  height: 100vh;
  border-bottom: 2px solid var(--color2);
}
#banner-title{
  color: #f5f7fa;
  font-family: 'Raleway';
  font-size: 65px;
  font-weight: 900;
  text-transform: uppercase;
  padding-top: 19%;
}
.desciption{
  color: #dedede;
  font-family: 'Raleway';
  font-size: 24px;
  line-height: 30px;
  padding: 20px;
}
.huit{
  color: white;
  height: 110px;
  width: 110px;
  border-radius: 55px;
  background: #d9534f;
  /*background: linear-gradient(120deg,  #db556f, var(--danger));*/
  /*background-color: var(--danger);*/
  box-shadow: 2px;
  position: absolute;
  top: 69%;
  right: 13%;
  box-shadow: 0 0 5px rgba(0,0,0,0.50);
/*  -webkit-transform: rotate(45deg);
  -moz-transform: rotate(45deg);
  -ms-transform: rotate(45deg);
  -o-transform: rotate(45deg);
  filter: progid:DXImageTransform.Microsoft.BasicImage(rotation=2);*/
}
.huit .text{
  margin-top: 38px;
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
  outline: 0;
}
.faa-bounce.animated{
  animation: bounce 2s ease infinite;
}
button:focus {outline:0;}
/*LES PLUS*/
#rassurance{
  position: relative;
  clear: both;
  text-align: center;
  font-size: 2.5em;
  padding: 45px;
  height: 100vh;
  background-color: var(--color4);
  /*border-bottom: 2px solid var(--color2);*/
}
#rassurance .about{
  padding: 45px;
  position: absolute;
  left: 25%;
  top: 45px;
  width: 50%;
}
#rassurance .about-border {
  content: '';
  background: var(--color2);
  height: 2px;
  width: 50%;
  margin-left: 25%;
  margin-top: 45px;
}
#rassurance .item{
  width: 33%;
  float: left;
  border-radius: 5px;
  margin-top: 45px;
  height: 200px;
}
.section-item{
  padding: 45px;
  margin-top: 250px;
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
  background-color: white;
}
#presentation .about{
  height: 120px;
  padding-top: 45px;
  margin-bottom: 45px;
}
#presentation .about-border {
  content: '';
  background: var(--color2);
  height: 2px;
  width: 16%;
  margin-left: 42%;
  margin-top: 45px;
}
#presentation .item{
  display: table;
  padding: 30px 0 30px 0;
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
  /*background-color: var(--color4);*/
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
  margin-left: -103px;
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
