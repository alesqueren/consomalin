import Vue from 'vue';
import { DiffPatcher } from 'jsondiffpatch/src/diffpatcher';
import resources from './resources';

const jsondiffpatch = new DiffPatcher();
const syncInterval = 1000;

let buffer = [];
let history = null;
let historyIndex = 0;
let previousState = {};

function deepCopy(data) {
  return JSON.parse(JSON.stringify(data));
}

function flush() {
  if (buffer.length > 0) {
    const unsavedMutations = deepCopy(buffer);
    buffer = [];
    resources.watcher.add(unsavedMutations);
  }
}

function storePlugin(store) {
  const startTime = (new Date()).getTime();

  setInterval(flush, syncInterval);

  store.subscribe((mutation, state) => {
    try {
      if (!history) {
        const delta = jsondiffpatch.diff(previousState, state);
        previousState = deepCopy(state);

        buffer.push({
          time: (new Date()).getTime() - startTime,
          mutation: mutation.type,
          delta,
        });
      }
    } catch (e) {
      // eslint-disable-next-line
      console.log(e);
    }
  });
}

function render() {
  const $ = window.$;
  Vue.nextTick(() => {
    if (history) {
      const t = history[historyIndex].time / 1000;
      const mutation = history[historyIndex].mutation;
      $('#replay').html('replay: ' + (historyIndex + 1) + ' / ' + history.length + ' ' + t + 's  ' + mutation);
    }
  });
}

function updateState(store, router, delta) {
  const oldRouteName = store.state.route.name;
  const stateCopy = deepCopy(store.state);
  const newState = deepCopy(jsondiffpatch.patch(stateCopy, delta));

  store.replaceState(newState);

  // force route change
  if (newState.route.name !== oldRouteName) {
    router.push({
      name: newState.route.name,
    });
  }
  render();
}

function init(store, router, sid) {
  resources.watcher.get({ sid }, {}).then(({ body }) => {
    history = JSON.parse(body).states;
    historyIndex = 0;

    // set initial state
    const newState = deepCopy(jsondiffpatch.patch({}, history[historyIndex].delta));
    store.replaceState(newState);

    updateState(store, router, undefined);
  });
}

function previous(store, router) {
  if (history && historyIndex > 0) {
    const delta = jsondiffpatch.reverse(history[historyIndex].delta);
    historyIndex -= 1;
    updateState(store, router, delta);
  }
}

function next(store, router) {
  if (history && historyIndex < (history.length - 1)) {
    historyIndex += 1;
    const delta = history[historyIndex].delta;
    updateState(store, router, delta);
  }
}

export default {
  storePlugin,
  init,
  previous,
  next,
};
