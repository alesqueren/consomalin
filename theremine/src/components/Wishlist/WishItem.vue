<template lang='pug'>
  div.wish.list-group-item.col-xs-6(v-on:click.stop="select")
      input(type="checkbox" v-model="selected" onclick="event.cancelBubble=true;")
      // div(v-if='editing')
      //   input(v-model="name")
      // div(v-else)
      //   span.groupName <strong>{{ name }}</strong>
      span {{ name }}
      button.btn.btn-primary.btn-sm(@click.stop="edit")
        i.fa.fa-pencil.fa-xs
      button.btn.btn-danger.btn-sm(v-on:click.stop="remove")
        i.fa.fa-trash-o.fa-xs
</template>

<script>

export default {
  props: ['id', 'gid', 'name', 'selected'],
  computed: {
    selectedWishes() {
      return this.$store.state.selectedWishes;
    },
    selected: {
      get() {
        const wishIsSelected = this.$store.getters.isSelectedWish(
          {
            gid: this.gid,
            wid: this.id,
          },
        );
        return wishIsSelected;
      },
      set(selected) {
        this.$store.dispatch('selectWish', {
          gid: this.gid,
          wid: this.id,
          selected,
        });
      },
    },
  },
  methods: {
    select() {
      this.$store.dispatch('selectWish', {
        gid: this.gid,
        wid: this.id,
        selected: !this.selected,
      });
    },

    edit() {
    },

    remove() {
      this.$store.dispatch('removeWish', {
        gid: this.gid,
        wid: this.id,
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
