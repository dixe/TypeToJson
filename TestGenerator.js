const Elm = require('./elm.min.js').Elm;

const app = Elm.Worker.init();

app.ports.stdout.subscribe((message ) =>{
    process.stdout.write(message)
});


const chunks = [];
process.stdin.on('data', (chunk) => {
    var data  = chunk.toString();
    app.ports.stdin.send(data)

});
