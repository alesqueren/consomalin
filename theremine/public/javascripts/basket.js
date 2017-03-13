Vue.component('wish-item', {
    props: ['wish'],
    template:
    `
        <div class="wish list-group-item col-xs-6">
            <div >
                {{wish.id}} : {{wish.name}} ( {{wish.groupId}}:{{wish.groupName}})
             </div>
             <div v-if="wish.product.infos.imageUrl">
                {{wish.product.infos.name}} : <img style="width:75px;height:75px;" v-bind:src="wish.product.infos.imageUrl">
                <input type="number" v-model.number="wish.product.quantity" step="1" value="0" min="1" max="64" v-on:change="changeQty" >
            </div>
        </div>
    `,
    methods: {
        changeQty: function () {
            $.ajax({
                type: 'PUT',
                url : '/wishlist/groups/'+this.wish.groupId+'/wishes/'+this.wish.id+'/product',
                data: {'qty' : this.wish.product.quantity },
                complete: function(responseObject) {
                }
            });
        }
    }
});
var app = new Vue({
    el: '#wishes',
    data: function() {
        return {
            wishGroups: wishGroups,
            pSelectedWishes: pSelectedWishes,
            maxProducts: 20,
            wishName: ''
        }
    },
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
    }
});