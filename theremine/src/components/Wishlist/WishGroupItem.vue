<template lang='pug'>
  div.wishgroup.list-group-item.col-3(v-bind:class="{'bg-info': selected}" @click="select")
    input(type="checkbox" v-model="selected" onclick="event.stopPropagation()")
    div
      span.groupName <strong>{{ wishgroup.name }}</strong>
    div
      button.btn.btn-danger.topright(@click.stop="remove")
        i.fa.fa-trash-o.fa-sm
    div
      button.btn.btn-primary.bottomright(@click.stop="editName")
        i.fa.fa-pencil.fa-sm
    div
      wishItem(v-for="wish in wishgroup.wishes" v-bind:wish="wish" v-bind:key="wish")
    div
      input(v-model="newWishName" v-on:keyup.enter="addWish" placeholder="Add a wish" onclick="event.stopPropagation()")
</template>

<script>
import wishItem from './WishItem';

function selectWish(context, selected) {
  const groupId = context.wishgroup.id;
  const store = context.$store.state;
  const currentBasket = store.currentBasket;
  const selectedWishes = currentBasket.selectedWishes;
  if (currentBasket && currentBasket.selectedWishes && !selected) {
    const selectedGroup = selectedWishes[groupId];
    for (const selectedWish in selectedGroup) {
      context.$store.dispatch('selectWish', {
        groupId,
        wishId: selectedWish,
        selected,
      });
    }
  } else {
    for (let i = 0; i < store.wishGroups.length; i++) {
      const wishgroup = store.wishGroups[i];
      if (wishgroup.id === groupId) {
        for (let j = 0; j < wishgroup.wishes.length; j++) {
          const wish = wishgroup.wishes[j];
          context.$store.dispatch('selectWish', {
            groupId,
            wishId: wish.id,
            selected,
          });
        }
      }
    }
  }
}

export default {
  props: ['wishlist', 'wishgroup', 'wishgroupindex'],
  data() {
    return {
      newWishName: '',
    };
  },
  computed: {
    selected: {
      get() {
        return this.$store.getters.isSelectedWishGroup(this.wishgroup.id);
      },
      set(selected) {
        this.select();
      },
    },
  },
  methods: {
    select() {
      this.$store.dispatch('selectWishGroup', {
        groupId: this.wishgroup.id,
        selected: !this.wishgroup.selected,
      });
    },
    addWish() {
      this.$store.dispatch('addWish', {
        group: this.wishgroup,
        name: this.newWishName,
      });
      this.newWishName = '';
    },
    remove() {
      this.$store.dispatch('removeWishGroup', this.wishgroup.id);
    },
    editName() {
    },
  },
  components: { wishItem },
};

</script>

<style scoped>
.wishgroup {
  position: relative;
  color: #00B7FF;
}
.groupName {
  font-size: 1.2em;
  color: black;
}
.topright {
  position: absolute;
  top: 0;
  right: 0;
}
.bottomright {
  position: absolute;
  bottom: 0;
  right: 0;
}
</style>
