import React, { useEffect } from "react";
import { BrowserRouter as Router, Switch, Route } from "react-router-dom";
import { useDispatch } from "react-redux";
import { actions, useTypedSelector } from "./store";
import { Link } from "react-router-dom";

const CircleRow = (props: { count: number }) => (
  <tr>
    {Array.from(Array(props.count)).map((_, i) => (
      <td key={i} className="text-center px-3 py-2">
        <input type="radio" />
      </td>
    ))}
  </tr>
);

const CircleGrid = (props: { label: string; columns: string[] }) => (
  <div>
    <h1 className="text-center text-2xl py-4">{props.label}</h1>
    <table className="mx-auto">
      <thead>
        <tr>
          {props.columns.map((x) => (
            <th key={x}>{x}</th>
          ))}
        </tr>
      </thead>
      <tbody>
        {Array.from(Array(props.columns.length)).map((_, i) => (
          <CircleRow key={i} count={props.columns.length} />
        ))}
      </tbody>
    </table>
  </div>
);

const App: React.FC = () => {
  const dispatch = useDispatch();
  const { isLoading } = useTypedSelector((state) => ({
    isLoading: state.isLoading,
  }));

  return (
    <Router>
      <Switch>
        <Route exact path="/">
          <CircleGrid label="Meditation" columns={["M", "T", "W", "Th", "F"]} />
          <CircleGrid label="Reading" columns={["M", "T", "W", "Th", "F"]} />
          <CircleGrid label="Challenge" columns={["M", "T", "W", "Th", "F"]} />
          <CircleGrid label="Jiu Jitsu" columns={["S", "M", "T"]} />
        </Route>
      </Switch>
    </Router>
  );
};

export default App;
