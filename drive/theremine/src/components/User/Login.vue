<template lang="pug">
div#login
  form
    h2 Connexion
    .border
    .prez
    .form-group
      span.fa.fa.fa-envelope.mail-icon
      input#username.form-control(v-model="username", type='text', name='username', tabindex='1', placeholder='Email', value='')
    .form-group
      span.fa.fa.fa-lock.password-icon
      input#password.form-control(v-model="password", type='password', name='password', tabindex='2', placeholder='Mot de passe')
    .form-group
      div.alert.alert-danger(v-if="error") Utilisateur ou mot de passe incorrect.
      input.submit.form-control.btn.btn-login(
            type.prevent='submit',
            name='login-submit',
            tabindex='4',
            value='Se connecter',
            @click="login")
    .login-link Nouveau sur Consomalin ?&nbsp;
      router-link(:to="{ name: 'register' }")
        span Créez un compte
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
    this.$store.dispatch('singleton/set', { registering: false });
  },
  computed: {
    registering() {
      return this.$store.state.singleton && this.$store.state.singleton.registering;
    },
  },
  methods: {
    loginM() {
      this.$store.dispatch('singleton/set', { registering: false });
    },
    registerM() {
      this.$store.dispatch('singleton/set', { registering: true });
    },
    fail() {
      this.error = true;
    },
    succeed() {
      this.$router.push({ name: 'wishlist' });
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
#login{
  text-align: center;
  font-size: 1.2em;
  border-radius: 2px;
  background: url('../../assets/images/main-cover.jpg') center top no-repeat;
  height: 100vh;
}
form{
  position: absolute;
  width: 500px;
  height: auto;
  top: 0;
  left: 50%;
  margin-left: -250px;
  background-color: white;
  border: 1px solid #dedede;
  padding: 30px 30px 25px 30px;
  margin-top: 60px;
}
#login h2 {
  padding-top: 10px;
  text-align: center;
}
.prez {
  margin-top: 55px
}
.login-link {
  margin-top: 35px
}
.login-link a {
  display: inline;
}
.mail-icon {
  position: absolute;
  right: 43px;
  top: 140px;
}
.password-icon {
  position: absolute;
  right: 45px;
  top: 195px;
}
.border{
  content: '';
  background: var(--color2);
  height: 2px;
  width: 130px;
  position: absolute;
  left: 50%;
  margin-left: -65px;
  margin-top: 5px;
}
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
.panel-login input:hover,
.panel-login input:focus {
  outline:none;
  -webkit-box-shadow: none;
  -moz-box-shadow: none;
  box-shadow: none;
  border-color: #ccc;
}
.btn-login {
  background-color: #1CA347;
  border-color: #1CA347;
  outline: none;
  color: #fff;
  font-size: 14px;
  height: auto;
  font-weight: normal;
  padding: 14px 0;
  text-transform: uppercase;
}
.btn-login:hover,
.btn-login:focus {
  color: #fff;
  background-color: #1CA347;
  border-color: #1CA347;
}
</style>
