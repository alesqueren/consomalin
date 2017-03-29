<template lang='pug'>
  div.wish.list-group-item.col-xs-6(v-on:click.stop="select")
      input(type="checkbox" v-model="selected" onclick="event.cancelBubble=true;")
      span {{ wish.name }}
      button.btn.btn-danger.right(v-on:click.stop="remove")
        i.fa.fa-trash-o.fa-lg
</template>

<script>

export default {
  props: ['wish'],
  computed: {
    selectedWishes() {
      return this.$store.state.selectedWishes;
    },
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
        this.$store.dispatch('selectWish', {
          groupId: this.wish.groupId,
          wishId: this.wish.id,
          selected,
        });
      },
    },
  },
  methods: {
    select() {
      this.$store.dispatch('selectWish', {
        groupId: this.wish.groupId,
        wishId: this.wish.id,
        selected: !this.wish.selected,
      });
    },

    remove() {
      this.$store.dispatch('removeWish', {
        groupId: this.wish.groupId,
        wishId: this.wish.id,
      });
    },
  },
};
</script>

<style scoped>
.wish button {
  visibility: hidden;
}
.wish:hover button {
  visibility: visible;
}
</style>
