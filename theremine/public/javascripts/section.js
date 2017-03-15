Vue.component('wish-item', {
    props: ['wish'],
    template:
    `
        <div class="wish list-group-item" style="padding:5px" @click="setCurrentWish()">
            <div>
                <span style="font-weight:bold">{{wish.groupName}}</span> {{wish.name}}
            </div>
            <div v-if="wish.product.infos.imageUrl">
                <div>
                    {{wish.product.infos.name}}
                </div>
                <img class="col-md-6" style="width:50px;" v-bind:src="wish.product.infos.imageUrl">
                <input class="col-md-6" type="number" v-model.number="wish.product.quantity" step="1" value="0" min="1" max="64" v-on:change="changeQty" >

            </div>
        </div>
    `,
    methods: {
        setCurrentWish: function () {
            this.$emit('new_current_wish', this.wish);
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
        <div class="input-group stylish-input-group">
            <input type="text" class="form-control" v-model.sync="currentwish.name" v-on:keyup.enter="rename" >
            <span class="input-group-addon">
                <button type="submit">
                    <span class="fa fa-search"></span>
                </button>  
            </span>
        </div>
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
        <div class="product-item list-group-item col-md-2" @click="selectProduct()">
             <img style="width:75px;height:75px;" v-bind:src="product.imageUrl">
             <span class="name">{{product.name}}</span>
             <span class="price">Prix : {{product.price}}€</span>
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
            maxProducts: 40,
            wishName: ''
        }
    },
    mounted:function(){
        this.searchProducts(this.currentWish);
    },
    created () {
        //on ecoute le scroll pour augmenter le nombre de produits visibles
        window.addEventListener('scroll', this.handleScroll);
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
        total: function(){
            return this.selectedWishes.reduce(function(prev, product){
              return prev + product.product.infos.price; 
            },0);
        }
    },
    methods: {
        sendCurrentWish : function (wish) {
            $.ajax({
                type: 'PUT',
                url : '/wishlist/groups/'+wish.groupId+'/wishes/'+wish.id+'/current',
                data: {},
                complete: function(responseObject) {
                    if ( responseObject.responseJSON == 'OK' ) {
                        // console.log('new current wish ('+ wish.name + ') accepted')
                    }else{
                        // console.log('new current wish ('+ wish.name + ') error')
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
            this.maxProducts = 40;
            //on notifie le serveur du nouveau wish courrant
            if ( this.currentWish != wish ) {
                this.currentWish = wish;
                this.sendCurrentWish(this.currentWish);
                //si on a aucun produit pour ce wish
                if (!Array.isArray(wish.matchingProducts) || wish.matchingProducts.length < 1) {
                    //on les recherche
                    this.searchProducts(this.currentWish);
                }else{
                    self.currentWish.matchingProducts = self.currentWish.matchingProducts;
                    // console.log(wish.name + ' already have products');
                    // console.log(wish.matchingProducts)
                }
            }else{
                // console.log(wish.name + ' same wish as ' + this.currentWish.name);
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
            if ( this.selectedWishes.length > currentIndex+1) {
                this.newCurrentWish(this.selectedWishes[currentIndex+1]);
            }else{
                // console.log(this.selectedWishes[currentIndex].name + ' est le dernier wish')
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