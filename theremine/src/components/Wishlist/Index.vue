<template lang="pug">
  div#wishlist.container-fluid
    div.row
      Groups.col-4
      ActiveGroup.col-4
      List.col-4
    router-link(:to='{ name: "basket" }')
      button.btn.btn-success.right(type="button", style="position:absolute;top:6px;right:51px;") Valider ma selection

</template>

<script>
import Groups from './Groups';
import ActiveGroup from './ActiveGroup';
import List from '../List/Index';

export default {
  components: { Groups, ActiveGroup, List },
};
</script>

<style scoped>
#wishlist {
  padding: 50px;
  font: 14px "Lucida Grande", Helvetica, Arial, sans-serif;
}
</style>
