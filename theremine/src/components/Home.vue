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
        div.num
          span 1
        div.img
          img.logo(src="../assets/images/liste.png")
        div.txt Fais ta liste de course comme à la maison
      div.item
        div.num
          span 2
        div.img
          img.logo(src="../assets/images/basket.png")
        div.txt Choisit tes produits en fonction de ta liste
      div.item
        div.num
          span 3
        div.img
          img.logo(src="../assets/images/withdraw.png")
        div.txt Régle tes achats chez Auchan lors du retrait
</template>

<script>
</script>

<style>
#homepage{
  padding: 50px 0 0 0;
}
#rassurance{
  clear: both;
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
  padding: 15px 0 15px 0;
}
#presentation .item:nth-child(odd){
  background-color: #2c3e60;
  color: white;
}
#presentation .item>div{
  display: table-cell;
  vertical-align: middle;
  height: 400px;
  width: 33%;
}
#presentation .item .num{
  vertical-align: center;
  border: 1px solid #2c3e60;
  border-radius: 100%;
  display: block;
  width: 70px;
  height: 70px;
  margin: 40% 0 0 50%;
  font-size: 2em;
}
#presentation .item:nth-child(odd) .num{
  background-color: white;
  color: #2c3e60;
}
#presentation .item:nth-child(even) .num{
  background-color: #2c3e60;
  color: white;
}
#presentation .item .img{
  min-width: 700px;
  min-height: 245.45px;
  background-color: white;
}
#presentation .item .img img{
  max-width: 700px;
}
#presentation .item .txt{
  text-align: center;
  padding-left: 25px;
  font-size: 1em;
}
</style>
