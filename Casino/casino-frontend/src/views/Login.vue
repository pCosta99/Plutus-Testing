<template>
  <div class="wrapper">
    <div class="inner">
      <form action="">
        <h3>Login</h3>
        <p style="font-size: x-large;" >The Art of Gaming, The Game of Art</p>
        <label class="form-group">
          <input type="text" class="form-control" v-model="input.wallet.number" required>
          <span>Wallet</span>
          <span class="border"></span>
        </label>
        <label class="form-group">
          <input type="text" class="form-control" v-model="input.wallet.cid" required>
          <span for="">Wallet Cid</span>
          <span class="border"></span>
        </label>
        <v-btn class="mx-2" fab dark color="indigo" @click="logIntoWallet">
          Login
          <v-icon dark>
            mdi-arrow-right
          </v-icon>
        </v-btn>
      </form>
    </div>
  </div>
</template>

<script>
export default {
  metaInfo: {
    title: 'Casino'
  },
  data () {
    return {
      input: {
        wallet: {
          number: undefined,
          cid: ""
        }
      }
    }
  },
  methods: {
    logIntoWallet () {
      if(this.input.wallet.number != undefined && this.input.wallet.cid != "") {
        if(this.$parent.wallets.includes(this.input.wallet.number)) {
          this.$emit("authenticated", true, this.input.wallet.cid);
          this.$router.replace({ name: "currency" });
        } else {
          console.log("Incorrect data!");
        }
      } else {
          console.log("Data must be present!");
      }
    }
  }
}
</script>

<style scoped src="../assets/login/styles/style.css">
</style>
