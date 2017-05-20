<template lang="pug">
  div.right(v-if='user === false')
    router-link(:to='{ name: "login" }')
      button.btn.btn-primary.connexion(type="button") Connexion
    router-link(:to='{ name: "register" }')
      button.btn.btn-primary.connexion(type="button") Inscription
  div.user.right(v-else)
    .dropdown
      #dropdownMenuButton.dropdown-toggle.user-name(data-toggle='dropdown', aria-haspopup='true', aria-expanded='false')
        span.fa.fa-user-o
      .dropdown-menu(aria-labelledby='dropdownMenuButton')
        a.dropdown-item.btn.btn-info.btn-sm.pointer(href="#")
          span {{ user }}
        div.dropdown-divider
        a.dropdown-item.btn.btn-info.btn-sm.pointer(href="#", @click.prevent='logout')
          span.fa.fa-sign-out 
          span Déconnexion

</template>

<script>
import { mapState } from 'vuex';

export default {
  computed: mapState({
    user: state => state.user.username,
  }),
  methods: {
    logout() {
      this.$store.dispatch('user/logout');
      this.$router.push({ name: 'home' });
    },
  },
};
</script>
<style>
div.right > a {
  display: inline;
}
.connexion{
  position: relative;
  cursor:pointer;
  margin-right: 10px;
}
.user{
  width: 200px;
}
.dropdown{
  display: table-cell;
  margin: 0;
  padding: 0;
  box-sizing: border-box;
  width: 200px;
  height: 50px;
  text-align: center;
  line-height: 50px;
  cursor: pointer;
}
.dropdown-item{
  background-color: var(--white);
  color: black;
}
.dropdown-item:hover{
  background-color: var(--active);
  color: black;
}
.dropdown-menu{
  /*left: -116px;*/
}
.user-name{
  color: white;
}
.pointer{
  cursor: pointer;
}
.dropdown:hover  {
  color:white !important;
  background-color: rgba(255, 255, 255, 0.14902);
}
</style>
