Vue.component('wishgroup-item', {
    props: ['wishgroups', 'wishgroup', 'wishgroupindex'],
    data: function() {
        return {
            newText: ''
        }
    },
    template:
        `
            <div class="wishgroup list-group-item col-xs-6">
                <button class="btn btn-danger" @click="removeWishGroup"><i class="fa fa-trash-o fa-lg"></i></button>
                {{ wishgroup.name }}
                <wish-item v-for="(wish, wishIndex) in wishgroup.wishes" v-bind:wishgroup="wishgroup" v-bind:wish="wish" v-bind:wishIndex="wishIndex" :key="wish.name"></wish-item>
                <input v-model="newText" v-on:keyup.enter="addWish" placeholder="Add a wish"/>
            </div>
        `,
    methods: {
        addWish: function () {
            var self = this;
            $.ajax({
                type: 'POST',
                url : '/wishlist/groups/'+self.wishgroup.id+'/wishes/bulk',
                data: { names : [ self.newText ] },
                complete: function(responseObject) {
                    self.wishgroup.wishes.push({id : responseObject.responseJSON, name:self.newText, selected:true});
                    self.newText = ''
                }
            });
        },

        removeWishGroup: function () {
            var self = this;
            // console.log(' gid : ' + gid)
            $.ajax({
                type: 'DELETE',
                url : '/wishlist/groups/'+self.wishgroupindex,
                data: {},
                complete: function(responseObject) {
                    self.wishgroups.splice(self.wishgroupindex, 1);
                }
            });
        }
    }
});
Vue.component('wish-item', {
    props: ['wishgroup', 'wish', 'wishIndex'],
    template:
    `
        <div class="wish list-group-item col-xs-6">
            {{ wish.name }}
            <input type="checkbox" v-model="wish.selected" v-on:change="selectWish">
        </div>
    `,
    methods: {
        selectWish: function () {
            var self = this;
            var gid = this.$parent.wishgroup.id;
            $.ajax({
                type: 'PUT',
                url : '/wishlist/groups/'+gid+'/wishes/'+this.wish.id,
                data: { selected: self.wish.selected},
                complete: function(responseObject) {
                    self.$parent.wishgroup.wishes[self.wishIndex].selected = self.wish.selected;
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
                    // console.log(responseObject);
                    self.wishGroups.push({
                        id:responseObject.responseJSON, 
                        name:self.newText,
                        wishes: []
                    });
                    self.newText = '';
                }
            });
        }
    }
});