<template lang="pug">
  div(style="position:relative;width:100%;")
    div#wishlist
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
      return config.MODE_DEMO;
    },
  },
  components: { Groups, ActiveGroup },
};
</script>

<style scoped>
#wishlist {
  font-size: 14px;
  padding: 30px 65px 65px 65px;
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
