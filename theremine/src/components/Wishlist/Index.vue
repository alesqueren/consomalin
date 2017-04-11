<template lang='pug'>
  div#wishlist.container-fluid
    div.row
      Groups.col-6
      ActiveGroup.col-6
</template>

<script>
import Groups from './Groups';
import ActiveGroup from './ActiveGroup';

export default {
  components: { Groups, ActiveGroup },
};
</script>

<style>
@font-face {
    font-family: gunny;
    src: url('/static/fonts/gnyrwn971.ttf');
}

#wishlist {
  padding: 50px;
  font: 14px "Lucida Grande", Helvetica, Arial, sans-serif;
}
</style>
