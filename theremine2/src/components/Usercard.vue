<template lang="pug">
  div(v-if='user === null')
    p .....
  div(v-else-if='user === false')
    span.fa.fa-sign-out
    router-link.connectRegister(to='/users/loginRegister') Se connecter S'inscrire
  div(v-else)
    span {{ user }}
    a.btn.btn-info.btn-sm(href='/users/signout')
    span.fa.fa-sign-out
    span Log out
</template>
<script>

import { mapState } from 'vuex';

export default {
  computed: mapState([
    'user',
  ]),
  mounted() {
    this.$store.dispatch('fetchUser');
  },
};
</script>
