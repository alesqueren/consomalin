<template lang="pug">
  div(style="position:relative;width:100%;")
    div#wishlist
      h2 Je crée et gére ma liste de courses
      div.content
          Groups.groups
          ActiveGroup.activegroup
          //- List.list
      router-link(:to='{ name: "basket" }')
        button.btn.btn-success.right.validation(type="button") Valider ma selection

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
  font: 14px "Lucida Grande", Helvetica, Arial, sans-serif;
}
#wishlist h2 {
  text-align: center;
}
#wishlist .content {
  width: 100%;
  display: table;
  padding-top: 50px;
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
#wishlist .validation {
  position:absolute;
  top:50px;
  right:50px;
}
</style>
