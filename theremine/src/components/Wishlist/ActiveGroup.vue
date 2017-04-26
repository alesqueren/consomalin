<template lang="pug">
  div
    div.activeGroup(v-if="wishgroup")
      div.notepad
        .redLine
        h2.title {{ wishgroup.name }}
        Wish(v-for="wish in wishgroup.wishes" 
          v-bind:wid="wish.id" 
          v-bind:gid="wishgroup.id" 
          v-bind:key="wish.id")
        input#newWish(v-model="newName" v-on:keyup.enter="add" placeholder="Ajouter un produit" @click.stop="")
        button.btn.btn-success.btn-sm.btn-create(v-if='creating' v-on:click="add")
          i.fa.fa-check.fa-xs
</template>

<script>
import Wish from './Wish';

const $ = window.$;

export default {
  props: [],
  data() {
    return {
      newName: '',
    };
  },
  computed: {
    creating() {
      return this.$store.state.singleton.action.type === 'createWish';
    },
    wishgroup() {
      const gid = this.$store.state.singleton.activeGroupId;
      if (gid) {
        return this.$store.getters['wishGroup/getGroup']({ gid });
      }
      return null;
    },
  },
  methods: {
    add() {
      this.$store.dispatch('wishGroup/addWish', {
        gid: this.wishgroup.id,
        name: this.newName,
      });
      this.newName = '';
    },
  },
  mounted() {
    $(document).click((event) => {
      if (!$(event.target).is('#newWish, .btn-create, .btn-edition')) {
        this.$store.dispatch('singleton/unset', {
          key: 'action',
        });
      }
    });
  },
  watch: {
    newName(val) {
      if (val) {
        this.$store.dispatch('singleton/set', {
          key: 'action',
          value: {
            type: 'createWish',
          },
        });
      } else {
        this.$store.dispatch('singleton/unset', {
          key: 'action',
        });
      }
    },
    creating(val) {
      if (!val) {
        this.newName = '';
      }
    },
  },
  components: { Wish },
};

</script>

<style scoped>
</style>
