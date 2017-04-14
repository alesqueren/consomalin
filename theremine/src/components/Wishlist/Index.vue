<template lang="pug">
  div#wishlist.container-fluid
    div.row
      Groups.col-6
      ActiveGroup.col-6
    router-link(:to='{ name: "basket" }')
      button.btn.btn-success.right(type="button", style="position:absolute;top:6px;right:51px;") Valider ma selection

</template>

<script>
import Groups from './Groups';
import ActiveGroup from './ActiveGroup';

export default {
  components: { Groups, ActiveGroup },
};
</script>

<style scoped>
@font-face {
    font-family: gunny;
    src: url('/static/fonts/gnyrwn971.ttf');
}

#wishlist {
  padding: 50px;
  font: 14px "Lucida Grande", Helvetica, Arial, sans-serif;
}
</style>
