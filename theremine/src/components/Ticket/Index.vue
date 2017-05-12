<template lang="pug">
div#ticket
  h2 Je valide mon ticket de caisse
  .box
    .group(v-for="gid in selectedGroups" 
      v-bind:gid="gid" 
      v-bind:key="gid")

    
</template>

<script>

export default {
  computed: {
    selectedGroups() {
      return this.$store.state.wishGroup.map(group => group.id);
    },
  },
  mounted() {
  },
  methods: {
  },
  components: {},
};
</script>

<style scoped>
</style>
