<template lang="pug">
</template>

<script>
import Vue from 'vue';
import buffer from '../buffer';

export default {
  mounted() {
    console.log(this.$store);
    // for (let i = 0; i < buffer.length; i += 1) {
    //   try {
    //     console.log(buffer[i].state.route.fullPath);
    //   } catch (e) {
    //     console.log('');
    //   }
    // }
    console.log(buffer[4].state.route.fullPath);
    this.$store.replaceState(buffer[4].state);
  },
};
</script>

<style>
</style>
