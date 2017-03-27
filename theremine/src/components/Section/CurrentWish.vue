<template lang='pug'>
  div.input-group.stylish-input-group(v-if="currentwish")
    input.form-control(type="text" v-model.sync="currentwish.name" v-on:keyup.enter="rename")
    span.input-group-addon
      button(type="submit")
        span.fa.fa-search
</template>

<script>
export default {
  props: ['currentwish'],
  methods: {
    rename: {
      // var currentgroupId = this.currentwish.groupId;
      // var currentwishId = this.currentwish.groupId;
      // var url = '/wishlist/groups/'+currentgroupId+'/wishes/'+currentwishId+'/rename';
      // var self = this;
      // $.ajax({
      //   type: 'PUT',
      //   url : url,
      //   data: {'name' : this.currentwish.name },
      //   complete: function(responseObject) {
      //     self.$emit('new_name', self.currentwish.name);
      //   }
      // });
    },
  },
};
</script>

<style scoped>
</style>
