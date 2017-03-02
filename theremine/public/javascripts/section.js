Vue.component('wishgroup-item', {
    props: ['wishgroup'],
    template:
        `
            <div>
                <wish-item v-for="(wish, wishIndex) in wishgroup.wishes" v-if="wish.selected == true" v-bind:wish="wish" v-bind:wishIndex="wishIndex" :key="wish.name"></wish-item>
            </div>
        `,
    methods: {
    }
});
Vue.component('wish-item', {
    props: ['wish', 'wishIndex'],
    template:
    `
        <div class="wish list-group-item col-xs-6">
            {{wish.name}}
        </div>
    `,
    methods: {
    }
});
var app = new Vue({
    el: '#wishgroups',
    data: {
        wishGroups: wishGroups
    }
});