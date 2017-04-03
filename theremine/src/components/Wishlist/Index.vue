<template lang='pug'>
  div#wishlist.container-fluid
    div.row
      Groups
      ActiveGroup
</template>

<script>
import Groups from './Groups';
import ActiveGroup from './ActiveGroup';

export default {
  components: { Groups, ActiveGroup },
};
</script>

<style scoped>
#wishlist {
  padding: 50px;
  font: 14px "Lucida Grande", Helvetica, Arial, sans-serif;
}
.no-gutter {
  margin-bottom:75px;
}
div.active-group {
  display: inline-block;
}
</style>
