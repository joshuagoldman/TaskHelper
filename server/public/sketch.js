
// ITP Networked Media, Fall 2014
// https://github.com/shiffman/itp-networked-media
// Daniel Shiffman

// Keep track of our socket connection
var socket;

function setup() {
  // Start a socket connection to the server
  // Some day we would run this server somewhere else
  socket = io.connect('http://localhost:3001');
  // We make a named event called 'mouse' and write an
  // anonymous callback function

  socket.on('message',
  // When we receive data
    function(data) {
        console.log(`This was sent back from da serva:\n${data}`);
    }
  );

  // Send that object to the socket
  socket.emit('message',"You just clicked the button");
}