<template>
  <p>{{ msg }}</p>
</template>

<script>
export default {
  data() {
    return {
      msg: 'my login',
    };
  },
};
</script>

<style scoped>
h1, h2 {
  font-weight: normal;
}

ul {
  list-style-type: none;
  padding: 0;
}

li {
  display: inline-block;
  margin: 0 10px;
}

a {
  color: #42b983;
}
</style>
