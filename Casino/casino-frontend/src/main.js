import Vue from 'vue'
import App from './App.vue'
import VueRouter from 'vue-router'
import Vuetify from 'vuetify'
import VueMaterial from 'vue-material'
import Routes from './routes'

Vue.config.productionTip = false
Vue.use(Vuetify)
Vue.use(VueMaterial)
Vue.use(VueRouter)

const router = new VueRouter({
  routes: Routes,
  mode: 'history'
});

new Vue({
  el: '#app',
  vuetify: new Vuetify(),
  render: h => h(App),
  router: router
})
