import '../css/styles.css'
import { Elm } from '../elm/Main.elm'

// Start the Elm application.
const app = Elm.Main.init({
  node: document.querySelector('main'),
  flags: {
    currentUrl: location.href,
  }
})

// Change between themes manually
app.ports.infoForJS.subscribe(function (message) {
  switch(message.tag) {
    case "setUrlParam":
      console.log(message);
      const { data } = message;
      const url = new URL(window.location);
      url.searchParams.set(data.key, data.value);

      window.history.pushState([], "", url);
      break;
    default:
      console.log(message);
  }
})
