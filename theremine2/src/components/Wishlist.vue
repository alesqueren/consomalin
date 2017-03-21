<template lang='pug'>
  p {{ msg }}
</template>

<script>
import resources from '../resources';

export default {
  data() {
    return {
      msg: this.getWishlist(),
    };
  },
  methods: {
    getWishlist() {
      resources.wishlist.get({});
      return 'my wishlist';
    },
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
