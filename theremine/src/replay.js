import resources from './resources';

let buffer = [];
let history = null;
let historyIndex = 0;

function flush() {
  if (buffer.length > 0) {
    const unsavedMutations = buffer;
    buffer = [];
    resources.watcher.save(unsavedMutations);
  }
}

function storePlugin(store) {
  const startTime = (new Date()).getTime();

  // setInterval(flush, 1000);

  store.subscribe((mutation, state) => {
    if (!history) {
      buffer.push({
        time: (new Date()).getTime() - startTime,
        // deep copy
        state: JSON.parse(JSON.stringify(state)),
      });
    }
  });
}

function updateState(store, router) {
  store.replaceState(history[historyIndex].state);
  if (history[historyIndex].state.route.name) {
    router.push({
      name: history[historyIndex].state.route.name,
    });
  }
}

function init(store, router) {
  history = buffer;
  historyIndex = 0;
  updateState(store, router);
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
