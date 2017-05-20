<template lang="pug">
  div.slotHour.classSlot(v-bind:class="{'disabled': pickupSlot.status == 'Past'}" @click="selectSlot()" v-bind:style="styleObject") {{frenchTime}}
</template>

<script>
function hue2rgb(p, q, t) {
  if (t < 0) t += 1;
  if (t > 1) t -= 1;
  if (t < 1 / 6) return p + (((q - p) * 6) * t);
  if (t < 1 / 2) return q;
  if (t < 2 / 3) return p + (((q - p) * ((2 / 3) - t)) * 6);
  return p;
}
function hslToRgb(h, s, l) {
  let r = 0;
  let g = 0;
  let b = 0;

  if (s === 0) {
    r = l;
    g = l;
    b = l;
  } else {
    const q = l < 0.5 ? l * (1 + s) : (l + s) - (l * s);
    const p = (2 * l) - q;
    r = hue2rgb(p, q, h + (1 / 3));
    g = hue2rgb(p, q, h);
    b = hue2rgb(p, q, h - (1 / 3));
  }

  return [Math.floor(r * 255), Math.floor(g * 255), Math.floor(b * 255)];
}
function numberToColorHsl(i) {
  // as the function expects a value between 0 and 1, and red = 0° and green = 120°
  // we convert the input to the appropriate hue value
  const hue = i * (1.2 / 360);
  // we convert hsl to rgb (saturation 100%, lightness 50%)
  const rgb = hslToRgb(hue, 1, 0.5);
  // we format to css value and return
  return 'rgb(' + rgb[0] + ',' + rgb[1] + ',' + rgb[2] + ')';
}
export default {
  props: ['pickupSlot'],
  computed: {
    classSlot() {
      return {
        'btn-secondary': !this.pickupSlot.selected,
        'btn-success': this.pickupSlot.selected,
      };
    },
    frenchTime() {
      const time = this.pickupSlot.time;
      const frenchTime = parseInt(time.split(':')[0], 10) + 'h' + time.split(':')[1];
      return frenchTime;
    },
    styleObject() {
      const attendance = 100 - (parseFloat(this.pickupSlot.attendanceLevel) * 100);
      const color = numberToColorHsl(attendance);
      const backgroundColor = color;
      return {
        backgroundColor,
      };
    },
  },
  methods: {
    selectSlot() {
      if (this.pickupSlot.status !== 'Past') {
        this.$store.dispatch('schedule/selectSlot', {
          slotId: this.pickupSlot.id,
          dateTime: this.pickupSlot.day + ' ' + this.pickupSlot.time,
        });
      }
    },
  },
};
</script>

<style scoped>
.slotHour{
  float:left;
  cursor: pointer;
  width: 60px;
  margin: 2px;
  color: var(--main-font);
  font-size: 15px;
  line-height: 25px;
  height: 25px;
  text-align: center;
  border: 1px solid rgba(0,0,0,0.25);
  border-radius: 2px;
}
.slotHour:not(.disabled):hover{
  font-weight: bolder;
}
.slotHour.disabled{
  opacity: 0.2;
  cursor: default;
}
</style>
