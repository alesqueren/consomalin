<template lang="pug">
  div(style="position:relative;width:100%;")
    div#wishlist
      .alert.alert-warning(v-if="uncheckingGroup")
        span.fa.fa-warning &nbsp;
        span Attention, vous avez déjà ajouté des produits de cette rubrique au panier, si vous la désélectionnnez, vous devrez les choisir de nouveau. <strong>Recochez la pour confirmer la désélection.</strong>
      .alert.alert-warning(v-if="uncheckingWish")
        span.fa.fa-warning &nbsp;
        span Attention, vous avez déjà ajouté ce produit au panier, si vous le désélectionnnez, vous devrez le choisir de nouveau. <strong>Recochez le pour confirmer la désélection.</strong>
      div.content
        Groups.groups
        ActiveGroup.activegroup
</template>

<script>
import config from '../../../config';
import Groups from './Groups';
import ActiveGroup from './ActiveGroup';

export default {
  computed: {
    demo() {
      return config.demo;
    },
    uncheckingGroup() {
      const action = this.$store.state.singleton.action;
      const type = action.type;
      return type === 'uncheckGroup';
    },
    uncheckingWish() {
      const action = this.$store.state.singleton.action;
      const type = action.type;
      return type === 'uncheckWish';
    },
  },
  components: { Groups, ActiveGroup },
};
</script>

<style scoped>
#wishlist {
  font-size: 14px;
  padding: 30px 65px 50px 65px;
}
#wishlist h2 {
  text-align: left;
}
#wishlist .content {
  width: 100%;
  display: table;
}
#wishlist .content .groups{
  display: table-cell;
  width: 49%;
  padding-right: 2%;
}
#wishlist .content .activegroup{
  display: table-cell;
  width: 49%;
}
#wishlist .content .list{
  width: 20%;
  display: table-cell;
}
.rightSide{
  position: fixed;
  top: 250px;
  right: 45px;
}
</style>
