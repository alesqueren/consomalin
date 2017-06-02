<template lang="pug">
  div.line(v-bind:class="{'active': isActive, 'strong': selectedWishNb}"
      @click="setActivation")
    div.confirmUncheck(v-if="unchecking")
      span.fa.fa-warning &nbsp;
    input(type="checkbox"
      name="selected"
      v-model="selected",
      :disabled="wishesNb === 0")
    input.edition(v-if='editing',
      ref="editinput",
      v-model="editingName",
      @click.stop="",
      @keyup.enter="validEdition",
      @keyup.esc="finishEdition")
    button.btn.btn-success.btn-sm.btn-edition(v-if='editing' @click="validEdition")
      i.fa.fa-check.fa-xs
    label.name(v-else for="selected")
      .nameIn {{ name }}
    div.fakeCheckbox(v-if='!editing && wishesNb' @click.stop.prevent="toggleSelection")
    div.filling
      span {{ selectedWishNb }} / {{ wishesNb }}
    .added.fa.fa-cart-arrow-down.tooltip(v-if="matchedWishesNb")
      span.tooltiptext.tooltip-bottom Vous avez ajouté des produits de cette rubrique au panier
    div.buttns(v-if='!editing')
      div.action.edit(@click.stop="startEdition")
        span.listenHover(data-action="edit")
        span.content Renommer&nbsp;
        span.icon.fa.fa-pencil(data-action="edit")
      div.action.delete(@click.stop="erase", v-bind:class="{'deleting': deleting}")
        span.listenHover(data-action="delete")
        span.icon.fa.fa-eraser
        span &nbsp;
        span.content {{deleteWording}}
    div.arrowActive(v-if="isActive")
</template>

<script>
import Vue from 'vue';

const $ = window.$;

export default {
  props: ['gid'],
  data() {
    return {
      editingName: null,
    };
  },
  computed: {
    name() {
      return this.$store.getters['wishGroup/getGroup']({ gid: this.gid }).name;
    },
    isActive() {
      return this.gid === this.$store.state.singleton.activeGroupId;
    },
    editing() {
      const action = this.$store.state.singleton.action;
      const gid = action.value ? action.value.gid : null;
      const type = action.type;
      return type === 'editGroup' && gid === this.gid;
    },
    deleting() {
      const action = this.$store.state.singleton.action;
      const gid = action.value && action.value.gid;
      const type = action.type;
      return type === 'deleteGroup' && gid === this.gid;
    },
    unchecking() {
      const action = this.$store.state.singleton.action;
      const gid = action.value && action.value.gid;
      const type = action.type;
      return type === 'uncheckGroup' && gid === this.gid;
    },
    selected() {
      return this.$store.getters['selection/getSelectedGroupsIds'].indexOf(this.gid) !== -1;
    },
    selectedWishNb() {
      return this.$store.getters['selection/getSelectedWishesByGroup']({ gid: this.gid }).length;
    },
    matchedWishesNb() {
      return this.$store.getters['selection/getMatchedWishesByGroup']({ gid: this.gid }).length;
    },
    wishesNb() {
      const predicate = e => (e.id === this.gid);
      return this.$store.state.wishGroup.filter(predicate)[0].wishes.length;
    },
    deleteWording() {
      return this.deleting ? 'Valider ?' : 'Effacer';
    },
  },
  methods: {
    toggleSelection() {
      const actionName = !this.selected ? 'selectGroup' : 'unselectGroup';
      if (actionName === 'unselectGroup' && this.matchedWishesNb && !this.unchecking) {
        this.startUncheck();
      } else {
        this.$store.dispatch('singleton/unset', 'action');
        this.$store.dispatch('selection/' + actionName, { gid: this.gid });
      }
    },
    unselectGroup() {
      this.$store.dispatch('selection/unselectGroup', { gid: this.gid });
    },
    setActivation() {
      this.$store.dispatch('singleton/set', { activeGroupId: this.gid });
    },
    focus() {
      this.$refs.editinput.focus();
    },
    erase() {
      if (!this.deleting) {
        this.startDeletion();
      } else {
        this.remove();
      }
    },
    startEdition() {
      this.editingName = this.name;
      this.$store.dispatch('singleton/set', {
        action: {
          type: 'editGroup',
          value: {
            gid: this.gid,
          },
        },
      });
      Vue.nextTick(this.focus);
    },
    finishEdition() {
      this.editingName = null;
      this.$store.dispatch('singleton/unset', 'action');
    },
    validEdition() {
      this.$store.dispatch('wishGroup/renameGroup', {
        gid: this.gid,
        name: this.editingName,
      });
      this.finishEdition();
    },
    startDeletion() {
      this.$store.dispatch('singleton/set', {
        action: {
          type: 'deleteGroup',
          value: {
            gid: this.gid,
          },
        },
      });
      this.setActivation();
    },
    startUncheck() {
      this.$store.dispatch('singleton/set', {
        action: {
          type: 'uncheckGroup',
          value: {
            gid: this.gid,
          },
        },
      });
      this.setActivation();
    },
    remove() {
      const gids = this.$store.state.wishGroup.map(group => group.id);
      const currentPosition = gids.indexOf(this.gid);
      this.$store.dispatch('wishGroup/removeGroup', { gid: this.gid });
      this.$store.dispatch('singleton/set', { activeGroupId: gids[currentPosition - 1] });
    },
  },
  mounted() {
    $(document)
      .on('mouseenter', '.action .listenHover', ({ target }) => {
        const $action = $(target).parent();
        const $btns = $action.parent();
        const action = $(target).data('action');
        const $btn = $btns.find('.' + action + '');
        const $content = $btn.find('.content');
        const backgroundColor = action === 'edit' ? 'white' : 'var(--danger)';
        const color = action === 'edit' ? 'black' : 'white';

        $(target).css({
          color,
        });
        $btn.css({
          border: '1px solid rgba(0,0,0,.25)',
          visibility: 'visible',
          backgroundColor,
          color,
          'z-index': '3',
        });
        $content.css({
          visibility: 'visible',
          backgroundColor,
          color,
        });
      })
      .on('mouseleave', '.action .listenHover', ({ target }) => {
        const $action = $(target).parent();
        const $btns = $action.parent();
        const action = $(target).data('action');
        const $btn = $btns.find('.' + action + '');
        const $content = $btn.find('.content');

        $(target).css({
          color: 'var(--main-font)',
        });
        $btn.css({
          border: '1px solid rgba(0,0,0,.01)',
          visibility: 'hidden',
          'background-color': 'none',
          color: 'var(--white)',
          'z-index': '1',
        });
        $content.css({
          visibility: 'hidden',
          'background-color': 'none',
          color: 'var(--main-font)',
        });
      });
  },
};
</script>

<style scoped>
.line.active {
  background-color: var(--active);
}
.arrowActive {
  content: " ";
  width: 0;
  height: 0;
  border-top: 25px solid transparent;
  border-bottom: 25px solid transparent;
  border-left: 20px solid var(--active);
  position: absolute;
  right: -19px;
  top: 0px;
}
.line.active label {
  text-decoration: underline;
}
.line:hover label {
  text-decoration: underline;
}
.filling {
  display: block;
  position: absolute;
  bottom: 0px;
  right: 5px;
  font-family: helvetica;
}
.strong{
  font-weight: bold;
}
.wrapper{
  height: 30px;
  vertical-align: middle;
  line-height: 20px;
  padding: 5px;
}
.listenHover{
/*  opacity: 0.5;
  background-color: blue;*/
  width: 20px;
  height: 20px;
  position: absolute;
  left: 0px;
  top: 0px;
  z-index: 100;
  visibility: visible;
}
.delete .listenHover{
  left: 0px;
  top: 0px;
}
.edit .listenHover{
  left: 64px;
  top: 0px;
}
.deleting{
  visibility: visible;
}
.nameIn{
  overflow: hidden;
  height: 50px;
}
.confirmUncheck{
  position: absolute;
  background-color: #f0ad4e;
  height: 49px;
  width: 65px;
  bottom: 0px;
  left: 0px;
  /*z-index: 1;*/
  font-family: helvetica;
  font-size: 13px;
  color: white;
  padding: 2px;
}
.confirmUncheck .fa-warning{
  position: absolute;
  bottom: 5px;
  right: 4px;
  font-size: 13px;
  width: 12px;
  height: 12px;
  color: black;
}
.added {
  position: absolute;
  bottom: 6px;
  right: 15px;
  height: 10px;
  font-size: 12px;
}
</style>
