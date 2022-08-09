import { createStore } from 'redux';

function counterReducer(
  state = { value: 0 },
  action: { type: string; payload: any }
) {
  switch (action.type) {
    case 'action1':
      return { value: state.value + 1 };
    case 'counter/decremented':
      return { value: state.value - 1 };
    default:
      return state;
  }
}

let store = createStore(counterReducer);

store.subscribe(() => console.log(store.getState()));

store.dispatch({ type: 'action1' });
store.dispatch({ type: 'action1' });
