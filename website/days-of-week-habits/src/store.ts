import { createSlice, configureStore, PayloadAction } from "@reduxjs/toolkit";
import { useSelector, TypedUseSelectorHook } from "react-redux";

// Monday    0
// Tuesday   1
// Wednesday 2
// Thursday  3
// Friday    4
// Saturday  5
// Sunday    6
type Day = number;

export interface Task {
  label: string;
  // Number of milliseconds
  duration: number;
}

export interface Habit {
  day: Day;
  tasks: Task[];
}

export interface State {
  isLoading: boolean;
  habits: Habit[];
}

const minute: number = 1000 * 60;
const hour: number = minute * 60;

/*******************************************************************************
 * Tasks
 ******************************************************************************/

const jiuJitsu: Task = {
  label: "Jiu Jitsu",
  duration: hour,
};

const hotYoga: Task = {
  label: "Hot Pod Yoga",
  duration: hour,
};

const shave: Task = {
  label: "Shave",
  duration: 10 * minute,
};

const vacuum: Task = {
  label: "Vacuum",
  duration: 15 * minute,
};

const nap: Task = {
  label: "Nap",
  duration: hour,
};

const trimNails: Task = {
  label: "Trim Nails",
  duration: 5 * minute,
};

const trash: Task = {
  label: "Take out trash",
  duration: 5 * minute,
};

const scheduleLaundry: Task = {
  label: "Schedule laundry pick-up",
  duration: 5 * minute,
};

/*******************************************************************************
 * Days
 ******************************************************************************/

const monday: Habit = {
  day: 0,
  tasks: [jiuJitsu],
};

const tuesday: Habit = {
  day: 1,
  tasks: [
    {
      label: "Work from 6PS",
      duration: hour * 8,
    },
    jiuJitsu,
  ],
};

const wednesday: Habit = {
  day: 2,
  tasks: [
    hotYoga,
    shave,
    {
      label: "Clean apartment sinks",
      duration: 15 * minute,
    },
  ],
};

const thursday: Habit = {
  day: 3,
  tasks: [],
};

const friday: Habit = {
  day: 4,
  tasks: [hotYoga],
};

const saturday: Habit = {
  day: 5,
  tasks: [vacuum, nap],
};

const sunday: Habit = {
  day: 6,
  tasks: [jiuJitsu, nap, shave, trimNails, trash, scheduleLaundry],
};

/*******************************************************************************
 * State
 ******************************************************************************/

const initialState: State = {
  isLoading: true,
  habits: [monday, tuesday, wednesday, thursday, friday, saturday, sunday],
};

export const { actions, reducer } = createSlice({
  name: "application",
  initialState,
  reducers: {
    toggleIsLoading: (state) => ({ ...state, isLoading: !state.isLoading }),
  },
});

/**
 * Defining and consuming this allows us to avoid annotating State in all of our
 * selectors.
 */
export const useTypedSelector: TypedUseSelectorHook<State> = useSelector;

export default configureStore({ reducer });
