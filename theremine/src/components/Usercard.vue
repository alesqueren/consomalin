<template lang="pug">
  div.right(v-if='user === false')
    router-link(:to='{ name: "login" }')
      button.btn.btn-primary.connexion(type="button") Connexion/Inscription
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
          span Déconnection

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
.connexion{
  position: relative;
  cursor:pointer;
  top: -2px;
  margin-right: 10px;
}
.dropdown{
  top: 16px;
  margin-right: 16px;
}
.dropdown-menu{
  left: -116px;
}
.user-name{
  color: #747e8f;
}
.pointer{
  cursor: pointer;
}
</style>
