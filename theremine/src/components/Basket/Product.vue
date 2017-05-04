<template lang="pug">
  div.product.full(
    @click='select()'
    v-if="productInfos")
    div.product-infos
      //- span.fa.fa-trash-o.fa-xs.product-erase(@click.prevent.stop='erase()')
      img.product-left(v-bind:src='productInfos.imageUrl')
      div.product-right
        div.product-name {{productInfos.name}}
        div.product-number
          div.count-input.space-bottom

            // - TOOO: make a component
            div.erase(v-if="deleting", @click.prevent.stop='erase()' href="#") Retirer
            a.incr-btn(@click.prevent.stop='addQuantity(-1)' href="#") –
            input.quantity(type='number', v-model.number='quantity', step='1', value='0', min='1', max='256' @click.prevent.stop='', disabled="disabled")
            a.incr-btn(@click.prevent.stop='addQuantity(1)' href="#") &plus;

          span.total &nbsp;&nbsp;&nbsp;&nbsp;{{total}}€

</template>

<script>
import router from '../../router';

export default {
  props: ['wid', 'pid'],
  data() {
    return {
      editingId: 'summary-' + this.wid,
    };
  },
  computed: {
    quantity() {
      const wish = this.$store.getters['wishGroup/getWish']({ wid: this.wid });
      const product = this.$store.getters['selection/getProduct']({
        gid: wish.gid,
        wid: this.wid,
        pid: this.pid,
      });
      return product.quantity;
    },
    // wish() {
    //   return this.$store.getters['wishGroup/getWish']({ wid: this.wid });
    // },
    productInfos() {
      return this.$store.state.product.details[this.pid];
    },
    total() {
      const total = this.productInfos.price * this.quantity;
      return parseFloat(total).toFixed(2);
    },
    deleting() {
      const action = this.$store.state.singleton.action;
      const wid = action.wid;
      const pid = action.pid;
      const type = action.type;
      return type === 'deleteProduct' && this.wid === wid && this.pid === pid;
    },
  },
  methods: {
    select() {
      this.$store.dispatch('sectionWishes/set', this.wid).then(() => {
        router.push({ name: 'section' });
      });
    },
    addQuantity(value) {
      const newQuantity = this.quantity + value;
      if (newQuantity >= 1 && newQuantity <= 64) {
        this.$store.dispatch('selection/updateProduct', {
          wid: this.wid,
          pid: this.pid,
          quantity: newQuantity,
        });
      } else if (newQuantity === 0) {
        this.startDeletion();
      }
    },
    startDeletion() {
      this.$store.dispatch('singleton/set', {
        action: {
          type: 'deleteProduct',
          wid: this.wid,
          pid: this.pid,
        },
      });
    },
    erase() {
      const wid = this.wid;
      this.$store.dispatch('selection/removeProduct', { wid, pid: this.pid });
    },
  },
};
</script>

<style scoped>
.product {
  cursor: pointer;
  position: relative;
  height: auto;
  min-width: 315px;
  width: 315px;
}

.product-infos {
  display: table;
}
.product-left {
  display: table-cell;
  width: 100px;
}
.product-right {
  position: relative;
  display: table-cell;
  vertical-align: middle;
  text-align: center;
  height: auto;
  width: 100%;
  padding: 5px;
  /*position: absolute;
  right: 20px;
  bottom: 50px;*/
}
.product-name {
  position: relative;
  max-height: 45px;
  overflow: hidden;
}
.product-number {
  position: relative;
  display: table;
  width: 100%;
}
.product {
  width: 100%;
  max-width: 100%;
}
.total{
  display: table-cell;
  width: 50%;
  height: 27px;
  line-height: 27px;
  font-size: 1.5em;
  font-weight: bold;
}
.buttons {
  position: absolute;
  top: 5px;
  right: 5px;
}
.count-input {
  position: relative;
  display: table-cell;
  width: 75px;
  max-width: 75px;
}
.count-input input {
  position: relative;
  width: 75px;
  height: 27px;
  line-height: 27px;
  border: 1px solid #000;
  border-radius: 2px;
  background: none;
  text-align: center;
}
/*.count-input input:focus {
  outline: none;
}*/
.count-input .incr-btn {
  display: block;
  position: absolute;
  width: 30px;
  height: 30px;
  font-size: 26px;
  font-weight: 300;
  text-align: center;
  line-height: 30px;
  top: 49%;
  right: 15px;
  margin-top: -15px;
  text-decoration:none;
}
input[type=number]::-webkit-inner-spin-button {
  -webkit-appearance: none;
}
.count-input .incr-btn:first-child {
  right: auto;
  left: 15px;
  z-index: 2;
}

.product-erase{
  visibility: hidden;
  position: absolute;
  font-size: 1.5em;
  top: 5px;
  right: 5px;
  color: #555;
  z-index: 1;
}
.erase{
  position: absolute;
  font-size: 13px;
  top: 0px;
  right: 13px;
  width: 76px;
  line-height: 27px;
  height: 27px;
  border-radius: 2px;
  color: white;
  z-index: 2;
  background-color: #d9534f;
  border: 1px solid #000;
}
.product:hover .product-erase{
  visibility: visible;
}
.product .product-erase:hover{
  color: #d9534f;
}
</style>
