import buffer from '../buffer';
import resources from '../resources';

// const commit(mutation, store) {
//   store.commit(buffer2[3]);
// };

// const flush = buffer => () => {
//   if (buffer.length > 0) {
//     const buffer2 = buffer;
//     buffer = [];
//     resources.watcher.save(buffer2);
//   }
// };

export default (store) => {
  const startTime = (new Date()).getTime();
  // const buffer = [];

  // setInterval(flush(buffer, store), 1000);

  store.subscribe((mutation, state) => {
    buffer.push({
      time: (new Date()).getTime() - startTime,
      // deep copy
      state: JSON.parse(JSON.stringify(state)),
    });
  });
};
