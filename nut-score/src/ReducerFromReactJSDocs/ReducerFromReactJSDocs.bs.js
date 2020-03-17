'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");

var initialState = /* record */[/* count */0];

function reducer(state, action) {
  if (action) {
    return /* record */[/* count */state[/* count */0] - 1 | 0];
  } else {
    return /* record */[/* count */state[/* count */0] + 1 | 0];
  }
}

function ReducerFromReactJSDocs(Props) {
  var match = React.useReducer(reducer, initialState);
  var dispatch = match[1];
  return React.createElement("div", undefined, React.createElement("div", undefined, "Count: ", String(match[0][/* count */0])), React.createElement("div", undefined, React.createElement("button", {
                      onClick: (function (_event) {
                          return Curry._1(dispatch, /* Decrement */1);
                        })
                    }, "-"), React.createElement("button", {
                      onClick: (function (_event) {
                          return Curry._1(dispatch, /* Increment */0);
                        })
                    }, "+")));
}

var make = ReducerFromReactJSDocs;

exports.initialState = initialState;
exports.reducer = reducer;
exports.make = make;
/* react Not a pure module */
