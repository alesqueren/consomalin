Vue.component('slot-item', {
    props: ['pickupslot'],
    template:
    `
        <div class="slot list-group-item col-xs-6" @click="initiateTransaction()">
            {{pickupslot.id}} : {{pickupslot.status}} ( {{pickupslot.day}}:{{pickupslot.time}})
        </div>
    `,
    computed: {
        selectedWishes: function () {
            var selectedWishes = [];
            for(var i = 0; i < wishGroups.length; i++ ) {
                var wishGroup = wishGroups[i];
                var wishGroupLength = wishGroup.wishes?wishGroup.wishes.length:0;
                for(var j = 0; j < wishGroupLength; j++ ) {
                    var wish = wishGroup.wishes[j];
                    var selected = pSelectedWishes[wishGroup.id]?pSelectedWishes[wishGroup.id][wish.id]?true:false:false;
                    if( selected ) {
                        wish.groupId = wishGroup.id;
                        wish.groupName = wishGroup.name;
                        wish.product.quantity = pSelectedWishes[wishGroup.id][wish.id].product?pSelectedWishes[wishGroup.id][wish.id].product.quantity:0;
                        selectedWishes.push(wish);
                    }
                }
            }
            return selectedWishes;
        },
    },
    methods: {
        initiateTransaction: function () {
            var data = {
                slot_id : this.pickupslot.id,
                slot_dateTime : this.pickupslot.day + this.pickupslot.time,
                wishes : this.selectedWishes
            }
            var self = this;
            $.ajax({
                type: 'POST',
                url : '/withdraw/select/',
                data: data,
                complete: function(responseObject) {
                    console.log('commande passé avec le slot : ' + self.pickupslot.id)
                }
            });
        }
    }
});
var app = new Vue({
    el: '#slots',
    data: function() {
        return {
            slots: [],
        }
    },
    mounted: function () {
        var self = this;
        $.ajax({
            method: 'GET',
            url: '/withdraw/search',
            success: function (data) {
                console.log(data.slots);
                self.slots = data.slots;
            },
            error: function (error) {
                alert(JSON.stringify(error));
            }
        });
    }
});