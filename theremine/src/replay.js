import Vue from 'vue';
import resources from './resources';

let buffer = [];
let history = null;
let historyIndex = 0;
const syncInterval = 1000;

function flush() {
  if (buffer.length > 0) {
    const lastUnsavedMutation = buffer[buffer.length - 1];
    buffer = [];
    resources.watcher.add(lastUnsavedMutation);
  }
}

function storePlugin(store) {
  const startTime = (new Date()).getTime();

  setInterval(flush, syncInterval);

  store.subscribe((mutation, state) => {
    if (!history) {
      buffer.push({
        time: (new Date()).getTime() - startTime,
        mutation: mutation.type,
        state: JSON.parse(JSON.stringify(state)), // deep copy
      });
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

function updateState(store, router) {
  const newState = history[historyIndex].state;
  store.replaceState(newState);
  if (newState.route.name) {
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
    updateState(store, router);
  });
}

function previous(store, router) {
  if (history && historyIndex > 0) {
    historyIndex -= 1;
    updateState(store, router);
  }
}

function next(store, router) {
  if (history && historyIndex < (history.length - 1)) {
    historyIndex += 1;
    updateState(store, router);
  }
}

export default {
  storePlugin,
  init,
  previous,
  next,
};
