Vue.component('wish-item', {
    props: ['wish'],
    template:
    `
        <div class="wish list-group-item col-xs-6">
             {{wish.id}} : {{wish.name}} ( {{wish.groupId}}:{{wish.groupName}})
        </div>
    `,
    methods: {
    }
});
Vue.component('currentwish-item', {
    props: ['currentwish'],
    template:
    `
        <input v-model="currentwish.name">
    `,
    methods: {
    }
});
Vue.component('products-item', {
    props: ['wish', 'product'],
    template:
    `
        <div class="product list-group-item col-xs-6">
             {{product.name}}
        </div>
    `,
    methods: {
    }
});
var app = new Vue({
    el: '#wishes',
    data: {
        wishGroups: wishGroups,
        pcurrentWish: '',
        wishName: '',
    },
    computed: {
        currentWish: {
          cache: false,
          get () {
                console.log('update currentWish');
                var currentWish = null;
                var firstSelected = true;
                for(var i = 0; i < wishGroups.length; i++ ) {
                    var wishGroup = wishGroups[i];
                    for(var j = 0; j < wishGroup.wishes.length; j++ ) {
                        var wish = wishGroup.wishes[j];
                        if( wish.selected ) {
                            if ( firstSelected && !pcurrentWish) {
                                console.log('cas1');
                                currentWish =  wish;
                                firstSelected = false;
                            }else if ( pcurrentWish && pcurrentWish.group == i && pcurrentWish.wish == j) {
                                console.log('cas2');
                                currentWish = wish;
                            }
                        }
                    }
                }
                return currentWish;
            }
        },
        wishesSelected: function () {
            console.log('update wishesSelected');
            var wishesSelected = [];
            var firstSelected = true;
            for(var i = 0; i < wishGroups.length; i++ ) {
                var wishGroup = wishGroups[i];
                for(var j = 0; j < wishGroup.wishes.length; j++ ) {
                    var wish = wishGroup.wishes[j];
                    if( wish.selected ) {
                        if ( firstSelected && !pcurrentWish) {
                            wish.current = true;
                            firstSelected = false;
                        }else if ( pcurrentWish && pcurrentWish.group == i && pcurrentWish.wish == j) {
                            wish.current = true;
                            firstSelected = false;
                        }else{
                            wish.current = false;
                        }
                        wish.groupId = wishGroup.id;
                        wish.groupName = wishGroup.name;
                        wish.id = j;
                        wishesSelected.push(wish);
                    }
                }
            }
          return wishesSelected;
        }
    },
    methods: {
        setCurrentWish: function () {
            var self = this;
            $.ajax({
                type: 'PUT',
                url : '/section/currentwish/'+currentWish.name,
                data: {},
                complete: function(responseObject) {
                    window.products = JSON.parse(responseObject.responseText);
                    console.log('products : ');
                    console.log(products);
                    currentWish.products = products;
                }
            });
        },
        getProducts: function () {
            var self = this;
            $.ajax({
                type: 'GET',
                url : '/products/search/'+currentWish.name,
                data: {},
                complete: function(responseObject) {
                    window.products = JSON.parse(responseObject.responseText);
                    console.log('products : ');
                    console.log(products);
                    currentWish.products = products;
                }
            });
        }
    },
    watch: {
        wishName: function () {
          this.answer = 'Waiting for you to stop typing...'
          this.getAnswer()
        },
        currentWish: function () {
            console.log('coucou');
            var self = this;
            $.ajax({
                type: 'PUT',
                url : '/section/currentwish/'+group.id +'/'+wish.id,
                data: {},
                complete: function(responseObject) {
                    var response = JSON.parse(responseObject.responseText);
                    console.log('response setcurrentwish: ' + response);
                }
            });
        },
    },

});