<template lang='pug'>
  .container
    p(v-if="error") Username or password not valid
    .row
      .col-md-6.offset-md-3
        .panel.panel-login
          .panel-body
            .row
              .col-lg-12
                form#login-form(@submit.prevent='login', role='form', style='display: block;')
                  .form-group
                    input#username.form-control(v-model="username" type='text', name='username', tabindex='1', placeholder='Email', value='')
                  .form-group
                    input#password.form-control(v-model="password" type='password', name='password', tabindex='2', placeholder='Mot de passe')
                  .form-group
                    .row
                      .col-sm-6.col-sm-offset-3
                        input#login-submit.form-control.btn.btn-login(type='submit', name='login-submit', tabindex='4', value='Se connecter')
</template>

<script>

export default {
  data() {
    return {
      username: '',
      password: '',
      error: false,
    };
  },
  methods: {
    loginFail() {
      this.error = true;
    },
    loginSucceed() {
      this.$router.replace('/');
    },
    login() {
      const data = {
        username: this.username,
        password: this.password,
      };
      this.$store.dispatch('login', {
        data,
        fail: this.loginFail,
        success: this.loginSucceed,
      });
    },
  },
};
</script>

<style scoped>
h1, h2 {
  font-weight: normal;
}

ul {
  list-style-type: none;
  padding: 0;
}

li {
  display: inline-block;
  margin: 0 10px;
}

a {
  color: #42b983;
}
</style>
