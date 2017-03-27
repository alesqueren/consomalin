<template lang='pug'>
  div.wish.list-group-item.col-xs-6(v-on:click.stop="select")
      button.btn.btn-danger.right(v-on:click.stop="remove")
        i.fa.fa-trash-o.fa-lg
      span {{ wish.name }}
      input(type="checkbox" v-model="selected" onclick="event.cancelBubble=true;")
</template>

<script>

export default {
  props: ['wish', 'wishIndex'],
  computed: {
    selectedWishes() {
      return this.$store.state.selectedWishes;
    },
    selected: {
      get() {
        // console.log('component:wishGroupItem:computed:selected:get: wishId');
        // console.log(this.wish.id);
        const wishIsSelected = this.$store.getters.isSelectedWish(
          {
            groupId: this.wish.groupId,
            wishId: this.wish.id,
          },
        );
        return wishIsSelected;
      },
      set(selected) {
        // console.log('component:wishGroupItem:computed:selected:set: wishId');
        // console.log(this.wish.id);
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
</style>
