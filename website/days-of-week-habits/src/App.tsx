import React, { useEffect } from "react";
import { BrowserRouter as Router, Switch, Route } from "react-router-dom";
import { useDispatch } from "react-redux";
import { actions, useTypedSelector } from "./store";
import { Link } from "react-router-dom";
import humanizeDuration from "humanize-duration";
import type { Task, Habit } from "./store";

const days = [
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday",
  "Sunday",
];

const postItColor = "yellow";

const PostIt = (props: { day: number; tasks: Task[] }) => (
  <div className={`bg-${postItColor}-300 mr-2 mb-2 w-1/5 px-4 pb-4`}>
    <h2 className="text-center py-4">{days[props.day]}</h2>
    {props.tasks.map((task) => (
      <ol key={task.label}>
        <li className="py-2">
          <span className={`text-${postItColor}-600 pr-2`}>
            [{humanizeDuration(task.duration)}]
          </span>
          {task.label}
        </li>
      </ol>
    ))}
  </div>
);

const App: React.FC = () => {
  const dispatch = useDispatch();
  const { isLoading, habits } = useTypedSelector((state) => ({
    isLoading: state.isLoading,
    habits: state.habits,
  }));

  return (
    <Router>
      <Switch>
        <Route exact path="/">
          <div className="mx-auto font-mono">
            <h1 className="text-center my-8">Habits</h1>
            <ol className="flex">
              {habits.map((habit) => (
                <PostIt key={habit.day} day={habit.day} tasks={habit.tasks} />
              ))}
            </ol>
          </div>
        </Route>
      </Switch>
    </Router>
  );
};

export default App;
