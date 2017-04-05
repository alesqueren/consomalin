<template lang="pug">
  div.right(v-if='user === false')
    router-link(:to='{ name: "login" }') Se connecter
    router-link(:to='{ name: "register" }') S'enregister
  div.right(v-else)
    span.user-name {{ user }}
    a.btn.btn-info.btn-sm(@click.prevent='logout')
      span.fa.fa-sign-out
      span Log out
</template>

<script>
import { mapState } from 'vuex';

export default {
  computed: mapState({
    user: state => state.user.user,
  }),
  methods: {
    logout() {
      this.$store.dispatch('user/logout');
      this.$router.push({ name: 'home' });
    },
  },
};
</script>
<style>
.user-name{
  color: #747e8f;
}
</style>
