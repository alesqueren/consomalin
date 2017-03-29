<template lang='pug'>
  div(v-if="currentwish")
    div Liste <b>{{ currentwish.groupName }}</b>
    div.input-group.stylish-input-group
      input.form-control(type="text" v-model="currentwish.name" v-on:keyup="rename")
      span.input-group-addon
        button(type="submit")
          span.fa.fa-search
</template>

<script>
export default {
  props: ['currentwish'],
  data() {
    return {
      delayTimer: null,
    };
  },
  computed: {
    name: {
      get() {
        return this.$store.getters.getCurrentWish.name;
      },
      set() {
        // set(name) {
        // this.$store.dispatch('renameWish', {
        //   groupId: this.currentwish.groupId,
        //   wishId: this.currentwish.id,
        //   name,
        // });
      },
    },
  },
  methods: {
    rename() {
      clearTimeout(this.delayTimer);
      this.delayTimer = setTimeout(() => {
        const name = this.currentwish.name;
        this.$store.dispatch('renameWish', {
          groupId: this.currentwish.groupId,
          wishId: this.currentwish.id,
          name,
        });
        this.$store.dispatch('searchProductsForName', { name });
      }, 200);
    },
  },
};
</script>

<style scoped>
</style>
