'use strict';

require('./styles.css');

const applicationUrl = 'http://localhost:8989';

const Elm = require('./Main');
const app = Elm.Main.fullscreen({ applicationUrl });
const jdp = require('./assets/jdp.js').create();
const uuid = require('uuid');

let prevState = null;

connect();

app.ports.setPrevState.subscribe(state => prevState = state);

function connect() {
    const source = new EventSource(applicationUrl + '/events');
    source.onmessage = (e) => app.ports.event.send(handleStateMutation(JSON.parse(e.data)));
    source.addEventListener('close-app', onClose);

    source.onopen = () => {
        console.log('connected');
        app.ports.connected.send(true);
        // prevState = null;
    };
    source.onerror = onClose;

    function onClose() {
        source.close();
        app.ports.connected.send(false);
        setTimeout(connect, 5000);
    }
}

function handleStateMutation(event) {
    if (event.event === 'update') {
        if (prevState) {
            event.diff = jdp.diff(prevState, event.state) || [];
        } else {
            event.diff = [];
        }
        prevState = event.state;
    }
    event.id = event.id || uuid.v4();
    return event;
}
