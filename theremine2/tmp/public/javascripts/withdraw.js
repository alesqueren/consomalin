Vue.component('day-item', {
    props: ['day','daykey', 'dayindex'],
    template:
    `
        <div class="col" v-if="day.name" >
            <h2> {{day.name}}</h2>
            <div v-for="slotHours in day.slots" :key="slotHours">
                <slot-item v-for="pickupslot in slotHours"  :key="pickupslot" v-bind:pickupslot="pickupslot"  v-on:select_slot="selectSlot"> </slot-item>
           
            </div>
        </div>
    `,
    methods: {
        selectSlot: function (slotId) {
            this.$emit('select_slot', slotId);
        }
    }
});
Vue.component('slot-item', {
    props: ['pickupslot'],
    template:
    `
        <button type="button" v-bind:disabled="pickupslot.status == 'Past'" v-bind:class="classSlot" class="btn" @click="selectSlot()" v-bind:style="styleObject">
            {{frenchTime}}
        </button>
    `,
    computed: {
        classSlot: function () {
            return {
                'btn-secondary': !this.pickupslot.selected,
                'btn-success': this.pickupslot.selected,
            }
        },
        frenchTime: function () {
            var time = this.pickupslot.time;
            var frenchTime = parseInt(time.split(':')[0],10) + 'h' + time.split(':')[1];
            return frenchTime;
        },
        styleObject: function () {
            var attendance = parseFloat(this.pickupslot.attendanceLevel);
            console.log(attendance)
            // vert rgb(130,234,109)
            // rouge rgb(245,20,9)
            var rmin = 130;
            var rdiff = 245 - 130;
            var gmin = 234;
            var gdiff = 234 - 20;
            var bmin = 109;
            var bdiff = 109 - 9;
            var r = parseInt(rmin + (rdiff * attendance),10);
            var g = parseInt(gmin - (gdiff * attendance),10);
            var b = parseInt(bmin - (bdiff * attendance),10);
            // var hsv = rgb2hsv(r,g,b);
            // backgroundColor = 'rgb('+hsv.h+','+hsv.s+','+hsv.v+')';
            backgroundColor = 'rgb('+r+','+g+','+b+')';
            console.log(backgroundColor)
            
            return {
                backgroundColor : backgroundColor
            }
        },
    },
    methods: {
        selectSlot: function () {
            if ( !this.pickupslot.selected ) {
                this.$emit('select_slot', this.pickupslot.id);
                var data = {
                    slot_id : this.pickupslot.id,
                    slot_dateTime : this.pickupslot.day + ' ' + this.pickupslot.time
                }
                $.ajax({
                    type: 'POST',
                    url : '/withdraw/select/',
                    data: data,
                    complete: function(responseObject) {
                    }
                });
            }
        }
    }
});
var app = new Vue({
    el: '#slots',
    data: function() {
        return {
            daySlots: null,
            loading: true,
            selectedSlot: null,
            disableNext: true,
            textNext: 'Valider ma commande',
        }
    },
    mounted: function () {
        var self = this;
        $.ajax({
            method: 'GET',
            url: '/withdraw/search',
            success: function (data) {
                self.daySlots = organizeSlotsPerDayAndHour(data.slots);
                self.loading = false;
            },
            error: function (error) {
                console.log(error);
                self.loading = false;
            }
        });
    },
    methods: {
        confirmSlot: function () {
            $.ajax({
                type: 'POST',
                url : '/withdraw/confirm/',
                data: {},
                complete: function(responseObject) {
                    window.location.href = "/confirmation";
                }
            });
        },
        selectSlot: function (slotId) {
            this.disableNext = false;
            for( var i = 0; i < this.daySlots.length; i++) {
                var day = this.daySlots[i];
                for(var j=0; j < day.slots.length;  j++){
                    var slotHours = day.slots[j];
                    if( slotHours ) {
                        for(var k=0; k < slotHours.length;  k++){
                            var slot = slotHours[k];
                            slot.selected = false;
                            if ( slot.id == slotId ) {
                                slot.selected = true;
                                var time = slot.time;
                                var frenchTime = day.name + ' ' + parseInt(time.split(':')[0],10) + 'h' + time.split(':')[1];
                            }
                        }
                    }
                }
            }
            this.textNext = 'Valider ma commande pour ' + frenchTime;
        }
    }
});
function organizeSlotsPerDayAndHour(slots){
    // console.log(slots);
    var days = ['Dimanche', 'Lundi', 'Mardi', 'Mercredi', 'Jeudi', 'Vendredi', 'Samedi'];
    var newSlots = [];
    var daySlots = [];
    for(var i = 0; i < slots.length; i++){
        slot = slots[i];
        slot.selected = false;

        var d = new Date(slot.day);
        var h = new Date(slot.day + ' ' + slot.time);
        //au dernier slot du jour, on les ajoutes tous
        if ( dayName != days[d.getDay()] || i+1 == slots.length && !i == 0) {
            newSlots.push({name : dayName, slots : daySlots});
            daySlots = [];
        }
        var dayName = days[d.getDay()];
        // console.log(slot.dayName);
        // if ( slots.hasOwnProperty(dayName) ) {
            // newSlots.push(dayName);
            // console.log(newSlots);
            // newSlots[dayName].slots = [];
        // }
        if ( !daySlots.hasOwnProperty(h.getHours()) ) {
            daySlots[h.getHours()] = [];
        }
        daySlots[h.getHours()].push(slot);
    }
    // console.log('slots remaniés')
    // console.log(newSlots)
    // newSlost = ['coucou','lundi','mardi']
    return newSlots;
}
function rgb2hsv (pr,pg,pb) {
    var rr, gg, bb,
        r = pr / 255,
        g = pg / 255,
        b = pb / 255,
        h, s,
        v = Math.max(r, g, b),
        diff = v - Math.min(r, g, b),
        diffc = function(c){
            return (v - c) / 6 / diff + 1 / 2;
        };

    if (diff == 0) {
        h = s = 0;
    } else {
        s = diff / v;
        rr = diffc(r);
        gg = diffc(g);
        bb = diffc(b);

        if (r === v) {
            h = bb - gg;
        }else if (g === v) {
            h = (1 / 3) + rr - bb;
        }else if (b === v) {
            h = (2 / 3) + gg - rr;
        }
        if (h < 0) {
            h += 1;
        }else if (h > 1) {
            h -= 1;
        }
    }
    return {
        h: Math.round(h * 360),
        s: Math.round(s * 100),
        v: Math.round(v * 100)
    };
}