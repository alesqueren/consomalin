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
var app = new Vue({
    el: '#wishes',
    data: {
        wishesSelected: wishesSelected
    }
});