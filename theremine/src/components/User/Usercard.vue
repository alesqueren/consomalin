<template lang="pug">
  div.right(v-if='user === false')
    router-link(:to='{ name: "login" }')
      button.btn.btn-primary.connexion(type="button") Connexion
    router-link(:to='{ name: "register" }')
      button.btn.btn-primary.connexion(type="button") Inscription
  div.right(v-else)
    .dropdown(style="line-height: 10px;")
      button#dropdownMenuButton.btn.btn-secondary.btn-sm.dropdown-toggle.user-name(type='button', data-toggle='dropdown', aria-haspopup='true', aria-expanded='false')
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
.dropdown{
  top: 11px;
  margin-right: 16px;
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
  left: -116px;
}
.user-name{
  color: white;
}
.pointer{
  cursor: pointer;
}
</style>
