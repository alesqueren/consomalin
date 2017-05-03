<template lang="pug">
  div#homepage
    div#title
      h2 Consomalin est un site de courses en ligne independant qui permet de commander chez Auchan drive sur une interface simple, rapide et sans publicité.
    div#rassurance
      div.item
        i.fa.fa-plus-circle
        span &nbsp;Rapide
      div.item
        i.fa.fa-plus-circle
        span &nbsp;Maitrise du panier
      div.item
        i.fa.fa-plus-circle
        span &nbsp;Aucune pub !
    div#presentation
      div.item
        div.num
          span 1
        div.img
          img.logo(src="../assets/images/liste.png")
        div.txt Fais ta liste de courses comme à la maison
      div.item
        div.num
          span 2
        div.img
          img.logo(src="../assets/images/basket.png")
        div.txt Le site te popose les produits en fonction de ta liste
      div.item
        div.num
          span 3
        div.img
          img.logo(src="../assets/images/withdraw.png")
        div.txt Régle tes achats chez Auchan lors du retrait
    div#registration(v-if="!user")
      router-link(:to='{ name: "register" }')
        button.btn.btn-primary.connexion(type="button") Inscription
    div#wishlist(v-else)
      router-link(:to='{ name: "section" }')
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
/*#content{
  background-image: url('../assets/images/fly.jpg');
  background-repeat: no-repeat;
  background-size: cover;
  background-position: center top;
  background-attachment: fixed;
}*/
#homepage{
  padding: 50px 0 0 0;
  font-weight: lighter;
}
#title{
  padding: 0 0 50px 0;
  text-align: center;
}
#rassurance{
  clear: both;
  text-align: center;
  font-size: 2.5em;
}
#rassurance .item{
  width: 33%;
  float: left;
  border-radius: 5px;
}

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
</style>
