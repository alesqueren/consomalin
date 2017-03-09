Vue.component('wish-item', {
    props: ['wish'],
    template:
    `
        <div class="wish list-group-item col-xs-6" @click="setCurrentWish()">
            <div >
                {{wish.id}} : {{wish.name}} ( {{wish.groupId}}:{{wish.groupName}})
             </div>
             <div v-if="wish.product.infos.name">
                {{wish.product.infos.name}} : <img style="width:75px;height:75px;" v-bind:src="wish.product.infos.imageUrl">
                <input type="number" v-model.number="wish.product.quantity" step="1" value="0" min="1" max="64" v-on:change="changeQty" >
            </div>
        </div>
    `,
    methods: {
        setCurrentWish: function () {
            var currentWish = this.wish;
            this.$emit('new_current_wish', currentWish);
        },
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
Vue.component('currentwish-item', {
    props: ['currentwish'],
    template:
    `
        <input v-model.sync="currentwish.name" v-on:keyup.enter="rename" >
    `,
    methods: {
        rename: function(){
            var self = this;
            $.ajax({
                type: 'PUT',
                url : '/wishlist/groups/'+this.currentwish.groupId+'/wishes/'+this.currentwish.id+'/rename',
                data: {'name' : this.currentwish.name },
                complete: function(responseObject) {
                    self.$emit('new_name', self.currentwish.name);
                }
            });
        }
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
            this.$emit('select_product', this.productkey, product);
        }
    }
});
var app = new Vue({
    el: '#wishes',
    data: function() {
        return {
            wishGroups: wishGroups,
            pSelectedWishes: pSelectedWishes,
            currentWish: currentWish,
            maxProducts: 20,
            wishName: ''
        }
    },
    mounted:function(){
        this.newCurrentWish(this.currentWish);
    },
    created () {
        //on ecoute le scroll pour augmenter le nombre de produits visibles
        window.addEventListener('scroll', this.handleScroll);
<<<<<<< HEAD
=======
        var selectedWishes = [];
        for(var i = 0; i < wishGroups.length; i++ ) {
            var wishGroup = wishGroups[i];
            var wishGroupLength = wishGroup.wishes?wishGroup.wishes.length:0;
            for(var j = 0; j < wishGroupLength; j++ ) {
                var wish = wishGroup.wishes[j];
                if( wish.selected ) {
                    wish.groupId = wishGroup.id;
                    wish.groupName = wishGroup.name;
                    wish.id = j;
                    selectedWishes.push(wish);
                }
            }
        }
>>>>>>> 6a9a02091fe1c76d515ba2e9db57d529a1cdd280
    },
    destroyed () {
        window.removeEventListener('scroll', this.handleScroll);
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
        sendCurrentWish : function (wish) {
            $.ajax({
                type: 'PUT',
                url : '/wishlist/groups/'+wish.groupId+'/wishes/'+wish.id+'/current',
                data: {},
                complete: function(responseObject) {
                    if ( responseObject.responseJSON == 'OK' ) {
                        console.log('new current wish accepted')
                    }else{
                        console.log('new current wish error')
                    }

                }
            });
        },

        searchProducts: function (wish) {
            var self = this;
            $.ajax({
                type: 'GET',
                url : '/products/search/'+wish.name,
                data: {},
                complete: function(responseObject) {
                    var products = JSON.parse(responseObject.responseText);
                    self.currentWish.matchingProducts = products;
                }
            });
        },

        newCurrentWish: function (wish) {
            var self = this;
            this.currentWish = wish;
            this.maxProducts = 20;
            //on notifie le serveur du nouveau wish courrant
            this.sendCurrentWish(this.currentWish);
            //si on a aucun produit pour ce wish
            if (wish.matchingProducts.length < 1) {
                //on les recherches
                this.searchProducts(this.currentWish);
            }
        },

        setCurrentWishToNext: function () {
            //on recupere l'index du wish courrant
            var currentIndex;
            for (i = 0; i < this.selectedWishes.length; i += 1) {
                if (this.selectedWishes[i] === this.currentWish) {
                    currentIndex = i;
                }
            }
            var groupCurrentWish = currentWish.groupId;
            var wishCurrentWish = currentWish.id;
            //si la longueur du tableau est plus grande on passe au suivant, rien sinon
            if ( this.selectedWishes.length > currentIndex) {
                this.currentWish = this.selectedWishes[currentIndex+1];
                this.newCurrentWish(this.currentWish);
            }else{
                console.log('on est au dernier wish')
            }
        },

        //on attache le produit (selectionné) au wish en cours
        bindCurrentWishWithProduct: function (key, product) {
            this.currentWish.product.id = key;
            this.currentWish.product.infos = product;
            this.currentWish.product.quantity = 1;
            const groupId = this.currentWish.groupId;
            const wishId = this.currentWish.id;
            $.ajax({
                type: 'POST',
                url : '/wishlist/groups/'+groupId+'/wishes/'+wishId+'/product',
                data: {'pid' : key },
                complete: function(responseObject) {
                    var products = JSON.parse(responseObject.responseText);
                    self.currentWish.matchingProducts = products;
                    self.maxProducts++;
                }
            });
            this.setCurrentWishToNext();
        },

        //lazyloading
        //lorsqu'on atteint le bas de la page a 100px pret on augmente le nombre de produits affichable
        handleScroll: function () {
            if($(window).scrollTop() + $(window).height() > $(document).height() - 100) {
               this.maxProducts += 20;
            }
        },
    },

});