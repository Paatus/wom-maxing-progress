import '../css/styles.css'
import { Elm } from '../elm/Main.elm'

// Start the Elm application.
const app = Elm.Main.init({
  node: document.querySelector('main')
})

// Change between themes manually
app.ports.infoForJS.subscribe(function (data) {
  switch(data.tag) {
    case "changeTheme":
      window.document.documentElement.setAttribute('data-theme', data)
      break;
  }
})
