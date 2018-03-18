'use strict';

require('./styles.css');

const Elm = require('./Main');
const app = Elm.Main.fullscreen();
const jdp = require('jsondiffpatch').create();
const uuid = require('uuid');

const source = new EventSource('http://localhost:8989/events');
source.onerror = () => source.close();
source.onmessage = (e) => app.ports.event.send(handleStateMutation(JSON.parse(e.data)));

let prevState = null;

function handleStateMutation(event) {
    if (event.event === 'update') {
        if (prevState) {
            event.delta = jdp.diff(prevState, event.state) || [];
        } else {
            event.delta = [];
        }
        prevState = event.state;
    }
    event.id = event.id || uuid.v4();
    return event;
}
