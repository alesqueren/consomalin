Vue.component('day-item', {
    props: ['day','daykey', 'dayindex'],
    template:
    `
        <div class="col-md-2" v-if="day.name" >
            <h2> {{day.name}}</h2>
            <slot-item v-for="pickupslot in day.slots" v-bind:pickupslot="pickupslot" :key="daykey" v-on:select_slot="selectSlot">
                // <span> {{daykey}}</span>
                // <span> {{dayindex}}</span>
                // <span> {{day.slots}}</span>
            </slot-item>
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
        <div v-bind:class="{ active: pickupslot.selected }" @click="selectSlot()" >
            {{pickupslot.status}} ({{pickupslot.time}})
        </div>
    `,
    methods: {
        selectSlot: function () {
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
                    // window.location.href = "/confirmation";
                }
            });
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
        }
    },
    mounted: function () {
        var self = this;
        $.ajax({
            method: 'GET',
            url: '/withdraw/search',
            success: function (data) {
                self.daySlots = organizeSlotsPerDay(data.slots);
                // console.log(self.slots)
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
            for( var i = 0; i < this.daySlots.length; i++) {
                var day = this.daySlots[i];
                for(var j=0; j < day.slots.length;  j++){
                    var slot = day.slots[j];
                    slot.selected = false;
                    if ( slot.id == slotId ) {
                        slot.selected = true;
                    }
                }
            }
        }
    }
});
function organizeSlotsPerDay(slots){
    // console.log(slots);
    var days = ['Dimanche', 'Lundi', 'Mardi', 'Mercredi', 'Jeudi', 'Vendredi', 'Samedi'];
    var newSlots = [];
    var daySlots = [];
    for(var i = 0; i < slots.length; i++){
        slot = slots[i];
        slot.selected = false;

        var d = new Date(slot.day);
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
        daySlots.push(slot);
    }
    // console.log('slots remaniés')
    // console.log(newSlots)
    // newSlost = ['coucou','lundi','mardi']
    return newSlots;
}