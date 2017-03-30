<template lang="pug">
  div(v-if='user === null')
    p .....
  div(v-else-if='user === false')
    router-link(:to='{ name: "login" }') Se connecter
    router-link(:to='{ name: "register" }') S'enregister
  div(v-else)
    span {{ user }}
    a.btn.btn-info.btn-sm(@click.prevent='logout')
      span.fa.fa-sign-out
      span Log out
</template>

<script>
import { mapState } from 'vuex';

export default {
  computed: mapState({
    user: state => state.User.user,
  }),
  methods: {
    logout() {
      this.$store.dispatch('logout');
      this.$router.push({ name: 'home' });
    },
  },
  mounted() {
    this.$store.dispatch('fetchUser');
  },
};
</script>
