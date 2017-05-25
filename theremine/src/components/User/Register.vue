<template lang="pug">
div#register
  form
    h2 Inscription
    .border
    .prez
    .form-group
      span.fa.fa.fa-envelope.mail-icon
      input.email.form-control(v-model="username", type='email', name='username', tabindex='1', placeholder='Email', value='')
    .form-group
      span.fa.fa.fa-lock.password-icon
      input.password.form-control(v-model="password", type='password', name='password', tabindex='2', placeholder='Mot de passe')
      input#newsletter(v-model="newsletter", type='checkbox', name='newsletter', tabindex='3')
      label.newsletter-label(for="newsletter") &nbsp;&nbsp;Je souhaite recevoir la newsletter Consomalin
    .form-group
      p.danger.danger-alert(v-if="error") Cet utilisateur existe deja.
      input.submit.form-control.btn.btn-register(
        type='submit',
        name='register-submit',
        tabindex='4',
        value="Créer un compte gratuitement",
        @click.prevent="register")
    .login-link Déjà un compte ?&nbsp;
      router-link(:to="{ name: 'login' }")
        span Connectez-vous
</template>

<script>
export default {
  data() {
    return {
      username: '',
      password: '',
      newsletter: false,
      error: false,
    };
  },
  mounted() {
    this.$store.dispatch('singleton/set', { registering: true });
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
      this.$router.push({ name: 'presentation' });
    },
    register() {
      const data = {
        username: this.username,
        password: this.password,
        newsletter: this.newsletter,
      };
      this.$store.dispatch('user/register', {
        data,
        fail: this.fail,
        success: this.succeed,
      });
    },
  },
};
</script>

<style scoped>
#register{
  text-align: center;
  font-size: 1.2em;
  border-radius: 2px;
  background: url('../../assets/images/brimstone-blue2.jpg') center top no-repeat;
  height: 100vh;
}
form{
  position: absolute;
  width: 500px;
  height: 415px;
  top: 0;
  left: 50%;
  margin-left: -250px;
  background-color: white;
  border: 1px solid #dedede;
  padding: 30px 30px 117px 30px;
  margin-top: 60px;
}
#register h2 {
  padding-top: 10px;
  text-align: center;
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
.login-link {
  margin-top: 35px
}
.login-link a {
  display: inline;
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
.prez {
  margin-top: 55px
}
#newsletter {
  margin-top: 20px;
}
.newsletter-label {
  cursor: pointer;
}
.submit {
  cursor: pointer;
  top: 195px;
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
