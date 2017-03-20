<template lang="pug">
  #app
    img(src="../assets/logo.png")
    div
      router-link(to="/") Go to home
    div
      router-link(to="/login") Go to login
    div
      router-link(to="/profile") Go to profile
    div
      span counter: {{ count }}
    div
      button(@click="add()") add
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
