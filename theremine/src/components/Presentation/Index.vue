<template lang="pug">
div#presentation
  h2 Bravo, vous avez fait le bon choix !
  h3 Bienvenue chez Consomalin.
  p Vous commencez déjà à reprendre votre consommation en main, Consomalin vous guidera pour constituer une liste de courses et vous aidera à choisir vos produit.
  p Votre liste de courses peut-être composées de recettes, de groupes (Barbecue, Pour bébé, Ménage) ou simplement de listes.
  .plus.one
    h3.text Utilisez votre liste de courses comme à la maison
    img(src="../../assets/images/myList.png")
  .plus.two
    h3.text Achetez seulement ce dont vous avez besoin en constituant un ensemble des produits dont vous pourriez avoir besoin
    img(src="../../assets/images/wishlist-800.png")
  .plus.three
    h3.text Ne deambulez plus dans les rayons.
    img(src="../../assets/images/wishlist-800.png")
    router-link(:to='{ name: "wishlist" }')
      span.input-group-addon.nav-btn.prefered
        span Commencer l'aventure !
</template>

<script>

export default {
  computed: {
  },
  mounted() {
  },
  methods: {
  },
  components: {},
};
</script>

<style scoped>
#presentation{
    margin: 45px;
    text-align: center;
}
.one{
    width: 700px;
    margin-left: 400px;
}
.one .text{
    width: 700px;
}
.one img{
    width: 150px;
}

.two{
    width: 700px;
    margin-left: 400px;
}
.two .text{
    width: 700px;
}
.two img{
    width: 400px;
}

</style>
