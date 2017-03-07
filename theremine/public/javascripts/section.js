Vue.component('wish-item', {
    props: ['wish'],
    template:
    `
        <div class="wish list-group-item col-xs-6" @click="setCurrentWish()">
            <div >
                {{wish.id}} : {{wish.name}} ( {{wish.groupId}}:{{wish.groupName}})
             </div>
             <div v-if="wish.productInfos.name">
                {{wish.productInfos.name}} : <img style="width:75px;height:75px;" v-bind:src="wish.productInfos.imageUrl">
            </div>
        </div>
    `,
    methods: {
        setCurrentWish: function () {
            var currentWish = this.wish;
            this.$emit('new_current_wish', currentWish);
        }
    }
});
Vue.component('currentwish-item', {
    props: ['currentwish'],
    data: function() {
        return {
            name: ''
        }   
    },
    template:
    `
        <input v-model.sync="currentwish.name" >
    `,
    methods: {
    }
});
Vue.component('products-item', {
    props: ['wish', 'productkey', 'product'],
    template:
    `
        <div class="product list-group-item col-xs-6" @click="selectProduct()">
             {{product.name}}
             <img style="width:75px;height:75px;" v-bind:src="product.imageUrl">
        </div>
    `,
    methods: {
        selectProduct: function ( product ) {
            var product = this.product;
            this.$emit('update_current_wish_product', this.productkey, product);
        }
    }
});
var app = new Vue({
    el: '#wishes',
    data: function() {
        return {
            wishGroups: wishGroups,
            currentWish: currentWish,
            maxProducts: 20,
            wishName: ''
        }
    },
    created () {
        window.addEventListener('scroll', this.handleScroll);
        var wishesSelected = [];
        for(var i = 0; i < wishGroups.length; i++ ) {
            var wishGroup = wishGroups[i];
            var wishGroupLength = wishGroup.wishes?wishGroup.wishes.length:0;
            for(var j = 0; j < wishGroupLength; j++ ) {
                var wish = wishGroup.wishes[j];
                if( wish.selected ) {
                    wish.groupId = wishGroup.id;
                    wish.groupName = wishGroup.name;
                    wish.id = j;
                    wishesSelected.push(wish);
                }
            }
        }
    },
    destroyed () {
        window.removeEventListener('scroll', this.handleScroll);
    },
    computed: {
        wishesSelected: function () {
            var wishesSelected = [];
            for(var i = 0; i < wishGroups.length; i++ ) {
                var wishGroup = wishGroups[i];
                var wishGroupLength = wishGroup.wishes?wishGroup.wishes.length:0;
                for(var j = 0; j < wishGroupLength; j++ ) {
                    var wish = wishGroup.wishes[j];
                    if( wish.selected ) {
                        wish.groupId = wishGroup.id;
                        wish.groupName = wishGroup.name;
                        wish.id = j;
                        wishesSelected.push(wish);
                    }
                }
            }
            return wishesSelected;
        },
    },
    methods: {
        newCurrentWish: function (newCurrentWish) {
            var self = this;
            this.currentWish = newCurrentWish;
            this.maxProducts = 20;
            if ( newCurrentWish.products.length < 1) {
                $.ajax({
                    type: 'GET',
                    url : '/products/search/'+newCurrentWish.name,
                    data: {},
                    complete: function(responseObject) {
                        var products = JSON.parse(responseObject.responseText);
                        self.currentWish.products = products;
                        self.maxProducts++;
                    }
                });
            }
        },
        bindCurrentWishWithProduct: function (key, product) {
            this.currentWish.productInfos = product;
            const groupId = this.currentWish.groupId;
            const wishId = this.currentWish.id;
            this.currentWish.product = key;
            $.ajax({
                type: 'POST',
                url : '/wishlist/groups/'+groupId+'/wishes/'+wishId+'/product',
                data: {'pid' : key },
                complete: function(responseObject) {
                    var products = JSON.parse(responseObject.responseText);
                    self.currentWish.products = products;
                    self.maxProducts++;
                }
            });
        },
        handleScroll: function () {
            // console.log("scroll detected");
            if($(window).scrollTop() + $(window).height() > $(document).height() - 100) {
               this.maxProducts += 20;
            }
        }
        // setCurrentWishToNext: function () {
        //     var currentWish = this.getCurrentWish();
        //     var groupCurrentWish = currentWish.groupId;
        //     var wishCurrentWish = currentWish.wishId;
        //     if ( wishGroups[groupCurrentWish].length > currentWish.wishId) {
        //         currentWish = wishGroups[groupCurrentWish][wishCurrentWish+1];
        //     }else{
        //         return currentWish;
        //     }
        // },
    },

});