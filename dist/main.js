const app = Elm.Main.init({
	node: document.getElementById('elm')
});

const socket = new WebSocket('wss://echo.websocket.org');
socket.addEventListener('open', () => {
	socket.send("hello");
});
socket.addEventListener('message', event => {
	console.log(event);
	app.ports.receiveMessage.send(event.data);
});
app.ports.sendMessage.subscribe(data => {
  socket.send(data);
});
