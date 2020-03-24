// This is the ReactJS documentation's useReducer example, directly ported over
// https://reactjs.org/docs/hooks-reference.html#usereducer

// Record and variant need explicit declarations.
type state = {count: int};

type action =
  | Increment
  | Decrement;

let initialState = {count: 0};

let reducer = (state, action) => {
  switch (action) {
  | Increment => {count: state.count + 1}
  | Decrement => {count: state.count - 1}
  };
};

[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, initialState);

  // We can use a fragment here, but we don't, because we want to style the counter
  <div>
    <div>
      {React.string("Count: ")}
      {React.string(string_of_int(state.count))}
    </div>
    <div>
      <button onClick={_event => dispatch(Decrement)}>
        {React.string("-")}
      </button>
      <button onClick={_event => dispatch(Increment)}>
        {React.string("+")}
      </button>
    </div>
  </div>;
};
