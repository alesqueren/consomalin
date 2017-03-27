<template lang='pug'>
  div.wishgroup.list-group-item.col-3(v-bind:class="{'bg-info' : selected}" @click="select")
    input(type="checkbox" v-model="selected")
    div
      button.btn.btn-danger.topright(@click.cancelBubble="remove")
        i.fa.fa-trash-o.fa-sm
    div
      button.btn.btn-primary.bottomright(@click.cancelBubble="editName")
        i.fa.fa-pencil.fa-sm
    div
      span.groupName <strong>{{ wishgroup.name }}</strong>
    div
      wishItem(v-for="(wish, wishIndex) in wishgroup.wishes" v-bind:wish="wish" v-bind:wishIndex="wishIndex" v-bind:key="wishIndex")
    div
      input(v-model="newWishName" v-on:keyup.enter="addWish" placeholder="Add a wish" onclick="event.cancelBubble=true;")
</template>

<script>
import wishItem from './WishItem';

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
        const wishGroupIsSelected = this.$store.getters.isSelectedWishGroup(
          {
            groupId: this.wishgroup.id,
          },
        );
        return wishGroupIsSelected;
      },
      set(selected) {
        const groupId = this.wishgroup.id;
        const store = this.$store.state;
        const selectedGroup = store.currentBasket.selectedWishes[groupId];
        if (!selected) {
          // console.log('component:wishGroupItem:computed:set: deselection');
          for (const selectedWish in selectedGroup) {
            // console.log('component:wishGroupItem:computed:set: selectWish :');
            // console.log(selectedWish);
            this.$store.dispatch('selectWish', {
              groupId,
              wishId: selectedWish,
              selected,
            });
          }
        } else {
          // console.log('component:wishGroupItem:computed:set: selection');
          for (let i = 0; i < store.wishGroups.length; i++) {
            const wishgroup = store.wishGroups[i];
            if (wishgroup.id === groupId) {
              for (let j = 0; j < wishgroup.wishes.length; j++) {
                const wish = wishgroup.wishes[j];
                // console.log('component:wishGroupItem:computed:set: wish.id :');
                // console.log(wish.id);
                this.$store.dispatch('selectWish', {
                  groupId,
                  wishId: wish.id,
                  selected,
                });
              }
            }
          }
        }
      },
    },
  },
  methods: {
    select() {
      // console.log('');
      // console.log('component:wishGroupItem:method:select: wishGroup.id');
      // console.log(this.wishgroup.id);
      const groupId = this.wishgroup.id;
      const selected = !this.wishgroup.selected;
      const store = this.$store.state;
      const selectedGroup = store.currentBasket.selectedWishes[groupId];
      if (!selected) {
        // console.log('component:wishGroupItem:method:select: deselection');
        for (const selectedWish in selectedGroup) {
          // console.log('component:wishGroupItem:method:select: selectWish :');
          // console.log(selectedWish);
          this.$store.dispatch('selectWish', {
            groupId,
            wishId: selectedWish,
            selected,
          });
        }
      } else {
        // console.log('component:wishGroupItem:method:select: selection');
        for (let i = 0; i < store.wishGroups.length; i++) {
          const wishgroup = store.wishGroups[i];
          if (wishgroup.id === groupId) {
            for (let j = 0; j < wishgroup.wishes.length; j++) {
              const wish = wishgroup.wishes[j];
              // console.log('component:wishGroupItem:method:select: wish.id :');
              // console.log(wish.id);
              this.$store.dispatch('selectWish', {
                groupId,
                wishId: wish.id,
                selected,
              });
            }
          }
        }
      }
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
