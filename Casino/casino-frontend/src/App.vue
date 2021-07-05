<template>
  <div id="app">
    <router-view @authenticated="setAuthenticated"/>
  </div>
</template>

<script>
export default {
  name: 'App',
  data () {
    return {
      authenticated: false,
      wallets: ["1","2","3","4"],
      walletCID: undefined
    }
  },
  mounted() {
    if(!this.authenticated) {
      this.$router.replace({ name: "login" });
    }
  },
  methods: {
    setAuthenticated(status, cid) {
      this.authenticated = status;
      this.walletCID = cid;
    },
    logout() {
      this.authenticated = false;
    }
  }
}
</script>

<style>
</style>
