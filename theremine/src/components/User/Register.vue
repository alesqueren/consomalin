<template lang="pug">
  .container
    .row
      .col-md-6.offset-md-3
        .panel.panel-login
          .panel-heading
            .row
              .col-xs-6.col-md-6
                a#login-form-link(
                  v-bind:class="{'active': !registering}",
                  @click="loginM",
                  href='#') Se connecter
              .col-xs-6.col-md-6
                a#register-form-link(
                  v-bind:class="{'active': registering}",
                  @click="registerM",
                  href='#') S'inscrire
            hr
          .panel-body
            .row
              .col-lg-12
                form#login-form(v-if="!registering")
                  .form-group
                    input#username.form-control(v-model="username", type='text', name='username', tabindex='1', placeholder='Email', value='')
                  .form-group
                    input#password.form-control(v-model="password", type='password', name='password', tabindex='2', placeholder='Mot de passe')
                  .form-group
                    p.danger.danger-alert(v-if="error") Utilisateur ou mot de passe incorrect.
                    .row
                      .col-sm-6.col-sm-offset-3
                        input#login-submit.submit.form-control.btn.btn-login(
                          type.prevent='submit',
                          name='login-submit',
                          tabindex='4',
                          value='Se connecter',
                          @click="login")
                form#register-form(v-if="registering")
                  .form-group
                    input#email.form-control(v-model="username", type='email', name='username', tabindex='1', placeholder='Email', value='')
                  .form-group
                    input#password.form-control(v-model="password", type='password', name='password', tabindex='2', placeholder='Mot de passe')
                  .form-group
                    p.danger.danger-alert(v-if="error") Cet utilisateur existe deja.
                    .row
                      .col-sm-6.col-sm-offset-3
                        input#register-submit.submit.form-control.btn.btn-register(
                          type='submit',
                          name='register-submit',
                          tabindex='4',
                          value="S'inscrire",
                          @click.prevent="register")
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
  mounted() {
    this.$store.dispatch('singleton/set', {
      key: 'registering',
      value: true,
    });
  },
  computed: {
    registering() {
      return this.$store.state.singleton && this.$store.state.singleton.registering;
    },
  },
  methods: {
    loginM() {
      this.$store.dispatch('singleton/set', {
        key: 'registering',
        value: false,
      });
    },
    registerM() {
      this.$store.dispatch('singleton/set', {
        key: 'registering',
        value: true,
      });
    },
    fail() {
      this.error = true;
    },
    succeed() {
      this.$router.push({ name: 'wishlist' });
    },
    register() {
      const data = {
        username: this.username,
        password: this.password,
      };
      this.$store.dispatch('user/register', {
        data,
        fail: this.fail,
        success: this.succeed,
      });
    },
    login() {
      const data = {
        username: this.username,
        password: this.password,
      };
      this.$store.dispatch('user/login', {
        data,
        fail: this.fail,
        success: this.succeed,
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
.submit {
  cursor: pointer;
}
.panel-login {
  border-color: #ccc;
  -webkit-box-shadow: 0px 0px 3px 1px rgba(0,0,0,0.2);
  -moz-box-shadow: 0px 0px 3px 1px rgba(0,0,0,0.2);
  box-shadow: 0px 0px 3px 1px rgba(0,0,0,0.2);
}
.panel-login>.panel-heading {
  padding-top: 10px;
  color: #00415d;
  background-color: #fff;
  border-color: #fff;
  text-align:center;
}
.panel-login>.panel-heading a{
  text-decoration: none;
  color: #666;
  font-weight: bold;
  font-size: 15px;
  -webkit-transition: all 0.1s linear;
  -moz-transition: all 0.1s linear;
  transition: all 0.1s linear;
}
.panel-login>.panel-heading a.active{
  color: #029f5b;
  font-size: 18px;
}
.panel-login>.panel-heading hr{
  margin-top: 10px;
  margin-bottom: 0px;
  clear: both;
  border: 0;
  height: 1px;
  background-image: -webkit-linear-gradient(left,rgba(0, 0, 0, 0),rgba(0, 0, 0, 0.15),rgba(0, 0, 0, 0));
  background-image: -moz-linear-gradient(left,rgba(0,0,0,0),rgba(0,0,0,0.15),rgba(0,0,0,0));
  background-image: -ms-linear-gradient(left,rgba(0,0,0,0),rgba(0,0,0,0.15),rgba(0,0,0,0));
  background-image: -o-linear-gradient(left,rgba(0,0,0,0),rgba(0,0,0,0.15),rgba(0,0,0,0));
}
.panel-login>.panel-body {
  padding: 10px;
}
.panel-login input[type="text"],.panel-login input[type="email"],.panel-login input[type="password"] {
  height: 45px;
  border: 1px solid #ddd;
  font-size: 16px;
  -webkit-transition: all 0.1s linear;
  -moz-transition: all 0.1s linear;
  transition: all 0.1s linear;
}
.panel-login input:hover,
.panel-login input:focus {
  outline:none;
  -webkit-box-shadow: none;
  -moz-box-shadow: none;
  box-shadow: none;
  border-color: #ccc;
}
.btn-login {
  background-color: #59B2E0;
  outline: none;
  color: #fff;
  font-size: 14px;
  height: auto;
  font-weight: normal;
  padding: 14px 0;
  text-transform: uppercase;
  border-color: #59B2E6;
}
.btn-login:hover,
.btn-login:focus {
  color: #fff;
  background-color: #53A3CD;
  border-color: #53A3CD;
}

.btn-register {
  background-color: #1CB94E;
  outline: none;
  color: #fff;
  font-size: 14px;
  height: auto;
  font-weight: normal;
  padding: 14px 0;
  text-transform: uppercase;
  border-color: #1CB94A;
}

.btn-register:hover,
.btn-register:focus {
  color: #fff;
  background-color: #1CA347;
  border-color: #1CA347;
}


</style>
