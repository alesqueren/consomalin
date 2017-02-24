Vue.component('wishgroup-item', {
    props: ['wishgroup'],
    data: function() {
        return {
            newWishText: ''
        }
    },
    template:
        `
            <div class="wishgroup list-group-item col-xs-6">
                {{ wishgroup.name }}
                <wish-item v-for="wish in wishgroup.wishes" v-bind:wish="wish" :key="wish.name"></wish-item>
                <input v-model="newWishText" v-on:keyup.enter="addNewWish( wishgroup.id )" placeholder="Add a wish"/>
            </div>
        `,
    methods: {
        addNewWish: function (index) {
            var self = this;
            $.ajax({
                type: 'POST',
                url : '/wishlist/groups/'+index+'/wishes/bulk',
                data: { names : [ self.newWishText ] },
                complete: function(responseObject) {
                    self.wishgroup.wishes.push({name:self.newWishText});
                    self.newWishText = ''
                }
            });
        }
    }
                // <button class="btn btn-danger btn-block" @click="removeWishGroup(wishgroup.id)"><i class="fa fa-trash-o fa-lg"></i></button>
});
Vue.component('wish-item', {
    props: ['wish'],
    template: '<div class="wish list-group-item col-xs-6">{{ wish.name }}</div>'
});
var app = new Vue({
    el: '#wishgroups',
    data: {
        newWishGroupText: '',
        wishGroups: wishGroups
    },
    methods: {
        addWishGroup: function (e) {
            var self = this;
            $.ajax({
                type: 'POST',
                url : '/wishlist/groups',
                data: { name : self.newWishGroupText},
                complete: function(responseObject) {
                    self.wishGroups.push({
                        id:$('.wishgroup').length, 
                        name:self.newWishGroupText,
                        wishes: []
                    });
                    self.newWishGroupText = '';
                }
            });
        },
        removeWishGroup: function (e) {
            var self = this;
            $.ajax({
                type: 'DELETE',
                url : '/wishlist/groups/'+gid,
                data: { id : self.newWishGroupText},
                complete: function(responseObject) {
                    self.wishGroups.remove({id:$('.wishgroup').length, name:self.newWishGroupText});
                    self.newWishGroupText = '';
                }
            });
        }
    }
});