<template lang='pug'>
  div
    button(type="button" v-bind:disabled="pickupslot.status == 'Past'" v-bind:class="classSlot" class="btn" @click="selectSlot()" v-bind:style="styleObject")
      span {{frenchTime}}
</template>

<script>
export default {
  props: ['pickupslot'],
  computed: {
    classSlot() {
      return {
        'btn-secondary': !this.pickupslot.selected,
        'btn-success': this.pickupslot.selected,
      };
    },
    frenchTime: () => {
      const time = this.pickupslot.time;
      const frenchTime = parseInt(time.split(':')[0], 10) + 'h' + time.split(':')[1];
      return frenchTime;
    },
    styleObject: () => {
      const attendance = parseFloat(this.pickupslot.attendanceLevel);
      console.log(attendance);
      // vert rgb(130,234,109)
      // rouge rgb(245,20,9)
      const rmin = 130;
      const rdiff = 245 - 130;
      const gmin = 234;
      const gdiff = 234 - 20;
      const bmin = 109;
      const bdiff = 109 - 9;
      const r = parseInt(rmin + (rdiff * attendance), 10);
      const g = parseInt(gmin - (gdiff * attendance), 10);
      const b = parseInt(bmin - (bdiff * attendance), 10);
      // var hsv = rgb2hsv(r,g,b);
      // backgroundColor = 'rgb('+hsv.h+','+hsv.s+','+hsv.v+')';
      const backgroundColor = 'rgb(' + r + ',' + g + ',' + b + ')';
      console.log(backgroundColor);
      return {
        backgroundColor,
      };
    },
  },
  methods: {
  },
};
</script>
