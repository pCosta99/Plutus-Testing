<template>
  <div>
    ADA: {{ currencies.ada }}
    BB:  {{ currencies.bb  }}
  </div>
</template>

<script>
import axios from 'axios';

export default {
  metaInfo: {
    title: 'Currency'
  },
  components:{
  },
  data () {
    return {
      cid: undefined,
      currencies: {
        ada: 0,
        bb: 0
      }
    }
  },
  mounted () {
    this.cid = this.$parent.walletCID;
    this.loadCurrencies();
    /*this.getStatus();*/
  },
  methods: {
    loadCurrencies () {
      const route = "http://localhost:3000/api/new/contract/instance/" + this.cid + "/endpoint/funds";
      axios.post(route).then(this.getStatus);
    },
    getStatus() {
      axios.get("http://localhost:3000/api/new/contract/instance/" + this.cid + "/status", {}).then(data => {
        console.log(data);
      });
    }
  }
}
</script>

<style scoped>
</style>
