<template lang="pug">
  div#homepage
    div#rassurance
      div.item + Rapide
      div.item + Moins de produits oubliés
      div.item + Aucune pub !
    div#presentation
      div.item
        div.img screenshot liste de course
        div.txt Fais ta liste de course comme à la maison
      div.item
        div.img screenshot de panier à moitié rempli
        div.txt Choisit tes produits en fonction de ta liste
      div.item
        div.img image de retrait de drive
        div.txt Régle tes achats chez Auchan lors du retrait
</template>

<script>
</script>

<style>
#rassurance{
  clear: both;
}
#rassurance .item{
  float: left;
}
#presentation{
  clear: both;
}
#presentation .item{
  clear: both;
}
#presentation .item>div{
  width: 50%;
}
#presentation .item .img{
  float: left;
}
#presentation .item .txt{
  float: right;
  text-align: left;
}
</style>
