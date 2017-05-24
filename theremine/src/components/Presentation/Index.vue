<template lang="pug">
div#presentation
  //- h2 Bravo, vous avez fait le bon choix !
  h2 Bienvenue sur Consomalin
  p.subTitle Reprenez le contrôle de vos courses.
  //- .border
  p La liste de courses est un outil formidable pour gagner du temps et faire des économies.
  p
    img(src="../../assets/images/wishlist.png")
  //- .plus.one
  //-   h3.text Utilisez votre liste de courses comme à la maison
  //-   img(src="../../assets/images/myList.png")
  //- .plus.two
  //-   h3.text Achetez seulement ce dont vous avez besoin en constituant un ensemble des produits necessaires.
  //-   img(src="../../assets/images/wishlist-800.png")
  //- .plus.three
  //-   h3.text Ne deambulez plus dans les rayons grâce à la recherche automatique.
  //-   img(src="../../assets/images/search.png")
  p Un barbecue à organiser ou une bonne recette dénichée sur Marmiton ?
  p Organisez vos produits grâce aux rubriques.
  router-link.begin(:to='{ name: "wishlist" }')
    span.input-group-addon.nav-btn.prefered
      span Ecrire une première liste de courses
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
    margin: 65px 65px 0 65px;
    text-align: center;
    font-size: 1.2em;
    background-color: white;
    border: 1px solid #dedede;
    border-radius: 2px;
    padding: 30px 30px 117px 30px;
}
.subTitle{
    font-size: 1.1em;
    margin-bottom: 40px;
}
.border {
  content: '';
  background: var(--color2);
  height: 2px;
  width: 20%;
  margin-left: 40%;
  margin-top: 20px;
  margin-bottom: 40px;
}
p {
    font-size: 1.2em;
}
img{
  width: 55%;
}
.begin{
    position: absolute;
    left: 50%;
    margin-left: -145px;
    padding-top: 35px;
    width: 290px;
    font-size: 1.3em;
    clear: both;
}
</style>
