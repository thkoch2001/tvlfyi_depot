import { createSlice, configureStore, PayloadAction } from "@reduxjs/toolkit";
import { useSelector, TypedUseSelectorHook } from "react-redux";

export interface State {
  isLoading: boolean;
}

const initialState: State = {
  isLoading: true,
};

export const { actions, reducer } = createSlice({
  name: "application",
  initialState,
  reducers: {
    toggleIsLoading: state => ({ ...state, isLoading: !state.isLoading }),
  }
});

/**
 * Defining and consuming this allows us to avoid annotating State in all of our
 * selectors.
 */
export const useTypedSelector: TypedUseSelectorHook<State> = useSelector;

export default configureStore({ reducer });
