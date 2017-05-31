<template lang="pug">
  div
    div.activeGroup(v-if="wishgroup")
      div.notepad
        .redLine
        h2.title {{ wishgroup.name }}
        Wish(v-for="wish in wishgroup.wishes" 
          v-bind:wid="wish.id" 
          v-bind:gid="wishgroup.id" 
          v-bind:key="wish.id")
        div.newIcon.fa.fa-plus.fa(@click="focus")
        div.hideOthers.hidden
        input#newWish(v-model="newName" v-on:keyup.enter="add" placeholder="De quoi avez vous besoin ?" @click.stop="")
        button.btn.btn-success.btn-sm.btn-create(v-if='creating' v-on:click="add")
          i.fa.fa-check.fa-xs
</template>

<script>
import Vue from 'vue';
import Wish from './Wish';

const $ = window.$;

function isElementInViewport(el) {
  el = el[0];
  let result = false;
  if (el) {
    const rect = el.getBoundingClientRect();
    result = (
      rect.top >= 0 &&
      rect.left >= 0 &&
      rect.bottom <= (window.innerHeight || document.documentElement.clientHeight) &&
      rect.right <= (window.innerWidth || document.documentElement.clientWidth)
    );
  }
  return result;
}

function positionInput() {
  if (!$('.activeGroup .line').length || isElementInViewport($('.activeGroup .line:last'))) {
    $('#newWish').css({
      position: 'relative',
      width: '100%',
    });
    $('.activeGroup .newIcon').css({
      position: 'absolute',
      left: '22px',
      'margin-left': '0',
    });
    $('.activeGroup .hideOthers').css({
      display: 'none',
    });
  } else {
    const activeGroupWidth = $('.activegroup .notepad').width();
    $('#newWish').css({
      position: 'fixed',
      bottom: '0',
      width: activeGroupWidth + 'px',
    });
    $('.activeGroup .newIcon').css({
      position: 'fixed',
      left: 'auto',
      'margin-left': '22px',
    });
    $('.activeGroup .hideOthers').css({
      display: 'inherit',
      width: activeGroupWidth + 'px',
    });
  }
}

export default {
  props: [],
  data() {
    return {
      newName: '',
    };
  },
  computed: {
    creating() {
      return this.$store.state.singleton.action.type === 'createWish';
    },
    wishgroup() {
      const gid = this.$store.state.singleton.activeGroupId;
      if (gid) {
        return this.$store.getters['wishGroup/getGroup']({ gid });
      }
      return null;
    },
  },
  methods: {
    focus() {
      $('#newWish').focus();
    },
    add() {
      if (this.newName) {
        this.$store.dispatch('wishGroup/addWish', {
          gid: this.wishgroup.id,
          name: this.newName,
        });
        this.newName = '';
      }
    },
  },
  mounted() {
    $(document).click((event) => {
      if (!$(event.target).is('#newWish, .btn-create, .btn-edition')) {
        this.$store.dispatch('singleton/unset', 'action');
      }
    });
    $('#newWish').focus();

    positionInput();
    $(document).scroll(() => {
      positionInput();
    });
  },
  watch: {
    newName(val) {
      if (val) {
        this.$store.dispatch('singleton/set', {
          action: {
            type: 'createWish',
          },
        });
      } else {
        this.$store.dispatch('singleton/unset', 'action');
      }
    },
    creating(val) {
      if (!val) {
        this.newName = '';
      }
    },
    wishgroup() {
      Vue.nextTick(() => {
        $('#newWish').focus();
        Vue.nextTick(() => {
          positionInput();
        });
      });
    },
  },
  components: { Wish },
};
</script>
<style>
.hideOthers{
  display: none;
  height: 75px;
  background : linear-gradient(to bottom, rgba(255,255,255,0), rgba(255,255,255,0.9));
  position: fixed;
  bottom: 50px;
  width: 100%;
  z-index: 3;
}
</style>
