'use strict';

require('./styles.css');

const Elm = require('./Main');
const app = Elm.Main.fullscreen();
const jdp = require('./assets/jdp.js').create();
const uuid = require('uuid');

let prevState = null;

const source = new EventSource('http://localhost:8989/events');
source.onmessage = (e) => app.ports.event.send(handleStateMutation(JSON.parse(e.data)));
source.onopen = () => {
    app.ports.connected.send(true);
    // prevState = null;
};
source.onerror = () => {
    app.ports.connected.send(false);
    // prevState = null;
};

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
