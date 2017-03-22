<template lang="pug">
  div(v-if='user === null')
    p .....
  div(v-else-if='user === false')
    span.fa.fa-sign-out
    router-link(to="/login") Se connecter
    router-link(to="/register") S'enregistrer
  div(v-else)
    span {{ user }}
    a.btn.btn-info.btn-sm(@click.prevent='logout')
    span.fa.fa-sign-out
    span Log out
</template>
<script>

import { mapState } from 'vuex';

export default {
  computed: mapState([
    'user',
  ]),
  methods: {
    logout() {
      this.$store.dispatch('logout');
      this.$router.replace('/');
    },
  },
  mounted() {
    this.$store.dispatch('fetchUser');
  },
};
</script>
