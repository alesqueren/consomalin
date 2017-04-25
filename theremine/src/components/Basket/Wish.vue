<template lang="pug">
  div.wish(
    @click='select()')
    span.wish-name(v-if="displayName") {{ wish.name }}
    span.fa.fa-check-square-o.fa-xs.wish-erase(
      @click.prevent.stop='erase()',
      v-if="displayName")
    Product(v-for="pid in productIds" 
      v-bind:pid="pid",
      v-bind:wid="wid",
      v-bind:key="pid")
</template>

<script>
import router from '../../router';
import Product from './Product';

export default {
  props: ['wid', 'displayName'],
  data() {
    return {
      editingId: 'summary-' + this.wid,
      editingName: null,
    };
  },
  computed: {
    wish() {
      return this.$store.getters['wishGroup/getWish']({ wid: this.wid });
    },
    productIds() {
      const pids = Object.keys(this.$store.state.selection[this.wish.gid][this.wish.id]);
      return pids;
    },
  },
  methods: {
    select() {
      this.$store.dispatch('currentWish/set', this.wid).then(() => {
        router.push({ name: 'section' });
      });
    },
    focus() {
      this.$refs.editinput.focus();
    },
    erase() {
      const wid = this.wid;
      const selected = false;
      this.$store.dispatch('selection/selectWish', { wid, selected });
    },
  },
  components: { Product },
};
</script>

<style scoped>
.wish {
  cursor: pointer;
  position: relative;
  float: left;
  height: auto;
  min-width: 320px;
  width: 320px;
  padding: 5px;
  background-color: color(--white);
  border: 1px solid grey;
}
.wish-name{
  font-family: gunny;
  font-size: 1.5em;
  font-weight: bold;
}
.wish:hover .wish-name{
  text-decoration: underline
}
.wish:hover{
  cursor: pointer;
  background-color: #f0f0f0;
  -webkit-transition: all 0.2s;
  -moz-transition:    all 0.2s;
  -ms-transition:     all 0.2s;
  -o-transition:      all 0.2s;
}
.wish-erase{
  visibility: hidden;
  position: absolute;
  font-size: 1.5em;
  top: 5px;
  right: 5px;
  z-index: 1;
}
.wish:hover .wish-erase{
  visibility: visible;
}
.wish .wish-erase:hover{
  color: var(--warning);
}
</style>
