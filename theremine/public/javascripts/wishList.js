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
                <wish-item v-for="(wish, wishIndex) in wishgroup.wishes" v-bind:wish="wish" v-bind:wishIndex="wishIndex" :key="wish.name"></wish-item>
                <input v-model="newWishText" v-on:keyup.enter="addNewWish( wishgroup.id )" placeholder="Add a wish"/>
            </div>
        `,
    methods: {
        addNewWish: function (gid) {
            var self = this;
            $.ajax({
                type: 'POST',
                url : '/wishlist/groups/'+gid+'/wishes/bulk',
                data: { names : [ self.newWishText ] },
                complete: function(responseObject) {
                    self.wishgroup.wishes.push({name:self.newWishText, selected:true});
                    self.newWishText = ''
                }
            });
        }
    }
                // <button class="btn btn-danger btn-block" @click="removeWishGroup(wishgroup.id)"><i class="fa fa-trash-o fa-lg"></i></button>
});
Vue.component('wish-item', {
    props: ['wish', 'wishIndex'],
    template:
    `
        <div class="wish list-group-item col-xs-6">
            {{ wish.name }}
            <input type="checkbox" v-model="wish.selected" v-on:change="selectWish( wishIndex, wish.selected )">
        </div>
    `,
    methods: {
        selectWish: function (wid, selected) {
            var self = this;
            var gid = this.$parent.wishgroup.id;
            $.ajax({
                type: 'PUT',
                url : '/wishlist/groups/'+gid+'/wishes/'+wid,
                data: { selected: selected},
                complete: function(responseObject) {
                    self.$parent.wishgroup.wishes[wid].selected = selected;
                }
            });
        }
    }
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

// $('body').on('click', '.wish input', function(){
//     var self = this;
//     $.ajax({
//         type: 'POST',
//         url : '/wishlist/groups/'+gid+'/wishes/'+wid,
//         data: { id : self.newWishGroupText},
//         complete: function(responseObject) {
//             self.wishGroups.remove({id:$('.wishgroup').length, name:self.newWishGroupText});
//             self.newWishGroupText = '';
//         }
//     });
// });