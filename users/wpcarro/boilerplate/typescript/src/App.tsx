import React, { useEffect } from "react";
import { BrowserRouter as Router, Switch, Route } from "react-router-dom";
import { useDispatch } from "react-redux";
import { actions, useTypedSelector } from "./store";
import { Link } from "react-router-dom";

const App: React.FC = () => {
  const dispatch = useDispatch();
  const { isLoading } = useTypedSelector(state => ({
    isLoading: state.isLoading,
  }));

  return (
    <Router>
      <nav className="bg-blue-400">
        <ul className="container mx-auto justify-between flex py-6 text-white">
          <li>
            <Link to="/">Home</Link>
          </li>
          <li>
            <Link to="/about">About</Link>
          </li>
          <li>
            <Link to="/contact">Contact</Link>
          </li>
        </ul>
      </nav>
      <Switch>
        <Route exact path="/">
          <div className="container mx-auto">
            <h1>Welcome to the home page. Loading: {isLoading ? "true" : "false"}</h1>
            <button
              className="bg-gray-300 py-4 px-6"
              onClick={() => dispatch(actions.toggleIsLoading())}>isLoading</button>
          </div>
        </Route>
        <Route exact path="/about">
          <div className="container mx-auto">
            <h1>Here is the about page.</h1>
          </div>
        </Route>
        <Route exact path="/contact">
          <div className="container mx-auto">
            <h1>Here is the contact page.</h1>
          </div>
        </Route>
      </Switch>
    </Router>
  );
};

export default App;
