Vue.component('wish-item', {
    props: ['wish'],
    template:
    `
        <div class="wish list-group-item col-xs-6" @click="setCurrentWish()">
             {{wish.id}} : {{wish.name}} ( {{wish.groupId}}:{{wish.groupName}})
        </div>
    `,
    methods: {
        setCurrentWish: function () {
            var self = this;
            var currentWish = self.wish;
            // this.currentWish = currentWish;
            // console.log(this)
            self.$emit('new_current_wish', currentWish);
            // console.log('end');
            // $.ajax({
            //     type: 'PUT',
            //     url : '/section/currentwish/' + currentWish.groupId + '/' + currentWish.id,
            //     data: {},
            //     complete: function(responseObject) {
            //         window.products = JSON.parse(responseObject.responseText);
            //         console.log('products : ');
            //         console.log(products);
            //         currentWish.products = products;
            //     }
            // });
        }
    }
});
Vue.component('currentwish-item', {
    props: ['currentwish'],
    data: function() {
        return {
            wishName: ''
        }   
    },
    template:
    `
        <input v-model.sync="currentwish.name" >
    `,
    methods: {
        newCurrentWish: function (newCurrentWish) {
            // console.log('current-wish-item methods receive newCurentWish');
          //   this.currentWish = newCurrentWish;
          //   var currentProducts = newCurrentWish.products?newCurrentWish.products:null;
          //   if ( !currentProducts ) {
          //       $.ajax({
          //           type: 'GET',
          //           url : '/products/search/'+currentWish.name,
          //           data: {},
          //           complete: function(responseObject) {
          //               window.products = JSON.parse(responseObject.responseText);
          //               console.log('products : ');
          //               console.log(products);
          //               this.currentWish.products = products;
          //           }
          //       });
          //   }
          // return currentProducts;
        }
    }
});
Vue.component('products-item', {
    props: ['wish', 'product'],
    template:
    `
        <div class="product list-group-item col-xs-6">
             {{product.name}}
             <img style="width:75px;height:75px;" v-bind:src="product.imageUrl">
        </div>
    `,
    methods: {
    }
});
var app = new Vue({
    el: '#wishes',
    data: function() {
        return {
            wishGroups: wishGroups,
            currentWish: currentWish,
            wishName: ''
        }
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
        // currentProducts: function () {
        //     var self = this;
        //     var currentProducts = currentWish.products?currentWish.products:null;
        //     if ( !currentProducts ) {
        //         $.ajax({
        //         type: 'GET',
        //         url : '/products/search/'+currentWish.name,
        //         data: {},
        //         complete: function(responseObject) {
        //             window.products = JSON.parse(responseObject.responseText);
        //             console.log('products : ');
        //             console.log(products);
        //             currentProductsself = products;
        //         }
        //     });
        //     }
        //   return currentProducts;
        // }
    },
    methods: {
        newCurrentWish: function (newCurrentWish) {
            // console.log('Vue methods receive newCurentWish');
            var self = this;
            this.currentWish = newCurrentWish;

            // var currentProducts = this.currentWish.products?this.currentWish.products:null;
            // if ( !currentProducts ) {
                $.ajax({
                    type: 'GET',
                    url : '/products/search/'+this.currentWish.name,
                    data: {},
                    complete: function(responseObject) {
                        var products = JSON.parse(responseObject.responseText);
                        self.currentWish.products = products;
                        // self.toto.tata = "done"
                    }
                });
            // }

        },
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
        getProducts: function () {
            // var self = this;
            // $.ajax({
            //     type: 'GET',
            //     url : '/products/search/'+currentWish.name,
            //     data: {},
            //     complete: function(responseObject) {
            //         window.products = JSON.parse(responseObject.responseText);
            //         console.log('products : ');
            //         console.log(products);
            //         currentWish.products = products;
            //     }
            // });
        }
    },
    // watch: {
    //     currentWish: function () {
    //       this.getProducts();
    //     }
    // },

});