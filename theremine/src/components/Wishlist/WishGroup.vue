<template lang='pug'>
  div.wishgroup.list-group-item.col-2(v-bind:class="{'bg-info': selected}" @click="select")
    // input(type="checkbox" v-model="selected")
    span.groupName <strong>{{ name }}</strong>
    span ({{ selectedWishesNb }} / {{ wishesNb }})
    button.btn.btn-danger.topright.titi(@click.stop="remove")
      i.fa.fa-trash-o.fa-sm
    button.btn.btn-primary.bottomright.titi(@click.stop="edit")
      i.fa.fa-pencil.fa-sm
</template>

<script>
export default {
  props: ['id', 'name'],
  computed: {
    selected() {
      return this.$store.getters.isSelectedWishGroup(this.id);
    },
    wishesNb() {
      return this.$store.state.wishGroups.filter(e => (e.id === this.id))[0].wishes.length;
    },
    selectedWishesNb() {
      const obj = this.$store.state.currentBasket.selectedWishes[this.id];
      if (obj) {
        return Object.keys(obj).length;
      }
      return 0;
    },
  },
  methods: {
    select() {
      this.$store.dispatch('selectWishGroup', {
        gid: this.id,
        selected: !this.selected,
      });
    },
    edit() { },
    remove() {
      this.$store.dispatch('removeWishGroup', this.id);
    },
  },
};
</script>


<style scoped>
.wishgroup button {
  visibility: hidden;
}
.wishgroup:hover button {
  visibility: visible;
}
</style>
