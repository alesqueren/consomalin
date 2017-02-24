Vue.component('wishgroup-item', {
    props: ['wishgroup'],
    template:
        `
            <div class="wishgroup list-group-item col-xs-6">
                {{ wishgroup.name }}
                <wish-item v-for="wish in wishgroup.wishes" v-bind:wish="wish"></wish-item>
                <input v-model="newWishText" v-on:keyup.enter="addNewWish( wishgroup.id )" placeholder="Add a wish"/>
            </div>
        `
});
Vue.component('wish-item', {
    props: ['wish'],
    template: '<div class="wish list-group-item col-xs-6">{{ wish.name }}</div>'
});
var wishgroups = new Vue({
    el: '#wishgroups',
    data: {
        newWishGroupText: '',
        wishGroups: wishGroups
    },
    methods: {
        addNewWishGroup: function (e) {
            var self = this;
            $.ajax({
                type: 'POST',
                url : '/wishlist/groups',
                data: { name : self.newWishGroupText},
                complete: function(responseObject) {
                    self.wishGroups.push({id:$('.wishgroup').length, name:self.newWishGroupText});
                    self.newWishGroupText = '';
                }
            });
        }
    }
});
var wishgroup = new Vue({
    el: '.wishgroup',
    data: {
        newWishText: '',
        wishGroups: wishGroups
    },
    methods: {
        addNewWish: function (index) {
            var self = this;
            $.ajax({
                type: 'POST',
                url : '/groups/'+gid+'/wishes/bulk',
                data: { name : self.newWishText},
                complete: function(responseObject) {
                    self.wishGroups[index].push({name:self.newWishText});
                    self.newWishText = ''
                }
            });
        }
    }
});