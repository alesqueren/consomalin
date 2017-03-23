<template lang='pug'>
  div.wish.list-group-item.col-xs-6
      button.btn.btn-danger.right(@click="removeWish")
        i.fa.fa-trash-o.fa-lg
      span {{ wish.name }}
      input(type="checkbox" v-model="selected")
</template>

<script>

export default {
  props: ['wish', 'wishIndex'],
  computed: {
    selected: {
      get() {
        const wishIsSelected = this.$store.getters.isSelectedWish(
          {
            groupId: this.wish.groupId,
            wishId: this.wish.id,
          },
        );
        return wishIsSelected;
      },
      set(selected) {
        this.$store.dispatch('selectWish', { groupId: this.wish.groupId, wish: this.wish, selected });
      },
    },
  },
  methods: {
    selectWish() {
      this.$store.dispatch('selectWish', {
        groupId: this.wish.groupId,
        wish: this.wish,
      });
    },

    removeWish() {
      this.$store.dispatch('removeWish', {
        groupId: this.wish.groupId,
        wishId: this.wish.id,
      });
    },
  },
};
</script>

<style scoped>
</style>
