<template lang="pug">
div#help
  div#presentation
    div.about
      div Comment Consommer Malin
      div.about-border
    div.item
      div.num
        span 1
      div.img
        img.logo(src="../../assets/images/list.png")
      div.txt Préparez votre liste de courses comme à la maison
    div.item
      div.num
        span 2
      div.img
        img.logo(src="../../assets/images/section.png")
      div.txt Le site vous popose les produits de votre liste
    div.item
      div.num
        span 3
      div.img
        img.logo(src="../../assets/images/retrait.png")
      div.txt Valider votre ticket de caisse et choisissez un horaire de retrait
    div.item
      div.num
        span 4
      div.img
        img.logo(src="../../assets/images/withdraw.png")
      div.txt {{ presText }}
</template>

<script>
import config from '../../../config';

export default {
  data() {
    return {
      demo: config.MODE_DEMO,
    };
  },
  computed: {
    presText() {
      let text = 'Réglez vos achats aux bornes de retrait Auchan.';
      if (this.demo) {
        text = 'Réglez vos achats aux bornes de retrait.';
      }
      return text;
    },
  },
  mounted() {
  },
  methods: {
  },
  components: {},
};
</script>

<style scoped>
#help{
  margin: 30px 45px 45px 30px;
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
  font-size: 0.7em;
}
</style>
