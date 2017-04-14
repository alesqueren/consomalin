<template lang="pug">
  div#homepage
    div#rassurance
      div.item
        i.fa.fa-plus-circle
        span &nbsp;Rapide
      div.item
        i.fa.fa-plus-circle
        span &nbsp;Moins de produits oubliés
      div.item
        i.fa.fa-plus-circle
        span &nbsp;Aucune pub !
    div#presentation
      div.item
        div.imgscreenshot liste de course
        div.txt Fais ta liste de course comme à la maison
      div.item
        div.img screenshot de panier à moitié rempli
        div.txt Choisit tes produits en fonction de ta liste
      div.item
        div.img
          img.logo(src="../assets/images/withdraw.png")
        div.txt Régle tes achats chez Auchan lors du retrait
</template>

<script>
</script>

<style>
#rassurance{
  clear: both;
  padding-top: 25px;
  text-align: center;
  font-size: 2.5em;
}
#rassurance .item{
  width: 33%;
  float: left;
}

#presentation{
  padding-top: 50px;
  clear: both;
  text-align: center;
  font-size: 2em;
}
#presentation .item{
  display: table;
}
#presentation .item>div{
  display: table-cell;
  width: 50%;
}
#presentation .item .img{
}
#presentation .item .txt{
  text-align: left;
  vertical-align: center;
}
</style>
