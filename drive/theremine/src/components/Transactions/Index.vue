<template lang="pug">
div#transactions
  div.transaction(v-for="transaction in transactions")
    | Commande passée le : {{ transactionFrenchDate(transaction.orderTime) }} <br/>
    | Total : {{ transaction.total }} <br/>
    | Produits : {{ transaction.products }} <br/>
    | Retrait à : {{ transactionFrenchDate(transaction.slot.dateTime) }} <br/>

</template>

<script>
import date from '../Utils/date';

export default {
  computed: {
    transactions() {
      return this.$store.state.transactions.transactions;
    },
  },
  methods: {
    transactionFrenchDate(thedate) {
      const t = date.toFrenchTime(new Date(thedate));
      return t.hours + 'h' + t.minutes + ' le ' + t.dayName + ' ' + t.day + ' ' + t.monthName + ' ' + t.year;
    },
  },
  created() {
  },
  components: {},
};
</script>

<style scoped>
#transactions{
  padding: 65px;
  font-size: 1.2em;
}
.transaction{
  margin-bottom: 15px;
  background-color: white;
  padding: 15px;
  border: 1px solid grey;
  border-radius: 6px;
}
</style>
