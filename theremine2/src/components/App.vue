<template lang="pug">
  #app
    div
      router-link(to="/") Go to home
    div
      router-link(to="/login") Go to login
    div
      router-link(to="/register") Go to register
    div
      router-link(to="/profile") Go to profile
    div
      span counter: {{ count }}
    div
      button(@click="add()") add
    hr
    router-view
</template>

<script>
import { mapState } from 'vuex';

export default {
  name: 'app',
  computed: mapState([
    'count',
  ]),
  methods: {
    add() {
      this.$store.dispatch('incrementA');
    },
  },
};
</script>

<style>
#app {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  margin-top: 60px;
}
</style>
