<template lang="pug">
  div(style="position:relative;width:100%;")
    div#wishlist
      .alert.alert-warning(v-if="uncheckingGroup")
        span.fa.fa-warning &nbsp;
        span Attention, vous avez déjà ajouté des produits de cette rubrique au panier, si vous la désélectionnnez, vous devrez les choisir de nouveau. <strong>Recochez la pour confirmer la désélection.</strong>
      .alert.alert-warning(v-if="uncheckingWish")
        span.fa.fa-warning &nbsp;
        span Attention, vous avez déjà ajouté ce produit au panier, si vous le désélectionnnez, vous devrez le choisir de nouveau. <strong>Recochez le pour confirmer la désélection.</strong>
      div.content
        Groups.groups
        ActiveGroup.activegroup
</template>

<script>
import config from '../../../config';
import Groups from './Groups';
import ActiveGroup from './ActiveGroup';

const $ = window.$;

function leaveAction(target) {
  const $action = $(target);
  const $btns = $action.parent();
  const action = $(target).find('.listenHover').data('action');
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
}

export default {
  computed: {
    demo() {
      return Boolean(config.demo === 'true');
    },
    uncheckingGroup() {
      const action = this.$store.state.singleton.action;
      const type = action.type;
      return type === 'uncheckGroup';
    },
    uncheckingWish() {
      const action = this.$store.state.singleton.action;
      const type = action.type;
      return type === 'uncheckWish';
    },
  },
  mounted() {
    // $(document)
    //   .on('mouseleave', '.action .listenHover', ({ target }) => {
    //     $('.action').each((index, element) => {
    //       leaveAction($(element));
    //     });
    //   });
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
      .on('mouseleave', '.action', () => {
        $('.action').each((index, element) => {
          leaveAction($(element));
        });
      });
  },
  components: { Groups, ActiveGroup },
};
</script>

<style scoped>
#wishlist {
  font-size: 14px;
  padding: 30px 65px 50px 65px;
}
#wishlist h2 {
  text-align: left;
}
#wishlist .content {
  width: 100%;
  display: table;
}
#wishlist .content .groups{
  display: table-cell;
  width: 49%;
  padding-right: 2%;
}
#wishlist .content .activegroup{
  display: table-cell;
  width: 49%;
}
#wishlist .content .list{
  width: 20%;
  display: table-cell;
}
.rightSide{
  position: fixed;
  top: 250px;
  right: 45px;
}
</style>
