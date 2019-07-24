const app = Elm.Main.init({
  node: document.getElementById("elm")
});

const socket = new WebSocket("ws://localhost:8080/ws");

socket.addEventListener("message", event => {
  try {
    const jsonData = JSON.parse(event.data);
    app.ports.receiveMessage.send(jsonData);
  } catch (err) {
    console.error(err);
  }
});

// app.ports.sendMessage.subscribe(data => {
//   socket.send(data);
// });
