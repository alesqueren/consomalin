<template lang="pug">
  div.wishgroup(v-if="selectedWishes.length")
    .group(@click.stop="setActivation")
      .line.groupName {{ name }}
      Wish(v-for="wid in selectedWishes" 
        v-bind:wid="wid" 
        v-bind:gid="gid" 
        v-bind:key="wid")
</template>

<script>
import Wish from './Wish';

const $ = window.$;

function manageScrollButton() {
  const th = $('#groups').height();
  const ch = $('#groups .wish.line:last').position().top;
  if (ch < th) {
    $('#list .bottom').hide();
  } else {
    $('#list .bottom').show();
  }
}

export default {
  props: ['gid'],
  data() {
    return {
      newWishName: '',
    };
  },
  computed: {
    name() {
      return this.$store.getters['wishGroup/getGroup']({ gid: this.gid }).name;
    },
    selectedWishes() {
      return this.$store.getters['selection/getSelectedWishesByGroup']({ gid: this.gid });
    },
  },
  methods: {
    setActivation() {
      this.$store.dispatch('singleton/set', { activeGroupId: this.gid });
      // router.push({ name: 'wishlist' });
    },
  },
  watch: {
    selectedWishes: () => {
      // TODO : Trouver un autre moyen d'attendre la nouvelle valeur/hauteur, nextTick ?
      setTimeout(() => {
        manageScrollButton();
      }, 200);
    },
  },
  mounted() {
    manageScrollButton();
    $('#groups').scroll(() => {
      manageScrollButton();
    });
  },
  components: { Wish },
};

</script>

<style scoped>
.groupName {
  font-size: 1em;
  font-weight: bold;
  text-align: center;
  border-bottom: 1px dotted #72c4ff;
}
</style>
