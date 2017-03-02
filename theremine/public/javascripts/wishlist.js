Vue.component('wishgroup-item', {
    props: ['wishgroup', 'wishgroups', 'wishgroupindex'],
    data: function() {
        return {
            newText: ''
        }
    },
    template:
        `
            <div class="wishgroup list-group-item col-xs-6">
                <button class="btn btn-danger" @click="removeWishGroup(wishgroupindex)"><i class="fa fa-trash-o fa-lg"></i></button>
                {{ wishgroup.name }}
                <wish-item v-for="(wish, wishIndex) in wishgroup.wishes" v-bind:wish="wish" v-bind:wishIndex="wishIndex" :key="wish.name"></wish-item>
                <input v-model="newText" v-on:keyup.enter="addWish( wishgroup.id )" placeholder="Add a wish"/>
            </div>
        `,
    methods: {
        addWish: function (gid) {
            var self = this;
            $.ajax({
                type: 'POST',
                url : '/wishlist/groups/'+gid+'/wishes/bulk',
                data: { names : [ self.newText ] },
                complete: function(responseObject) {
                    self.wishgroup.wishes.push({name:self.newText, selected:true});
                    self.newText = ''
                }
            });
        },

        removeWishGroup: function (gid) {
            var self = this;
            // console.log(' gid : ' + gid)
            $.ajax({
                type: 'DELETE',
                url : '/wishlist/groups/'+gid,
                data: {},
                complete: function(responseObject) {
                    self.wishgroups.splice(gid, 1);
                }
            });
        }
    }
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
        newText: '',
        wishGroups: wishGroups
    },
    methods: {
        addWishGroup: function (e) {
            var self = this;
            $.ajax({
                type: 'POST',
                url : '/wishlist/groups',
                data: { name : self.newText},
                complete: function(responseObject) {
                    self.wishGroups.push({
                        id:$('.wishgroup').length, 
                        name:self.newText,
                        wishes: []
                    });
                    self.newText = '';
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