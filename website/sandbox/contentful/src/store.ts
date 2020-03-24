import { createSlice, configureStore, PayloadAction } from "@reduxjs/toolkit";
import { useSelector, TypedUseSelectorHook } from "react-redux";

export interface Book {
  title: string;
  author: string;
  // TODO(wpcarro): Prefer datetime type here.
  publicationDate: string;
}

export interface State {
  isLoading: boolean;
  books: Book[];
}

const initialState: State = {
  isLoading: true,
  books: [],
};

export const { actions, reducer } = createSlice({
  name: "application",
  initialState,
  reducers: {
    toggleIsLoading: state => ({ ...state, isLoading: !state.isLoading }),
    setBooks: (state, action) => ({ ... state, books: action.payload }),
  }
});

/**
 * Defining and consuming this allows us to avoid annotating State in all of our
 * selectors.
 */
export const useTypedSelector: TypedUseSelectorHook<State> = useSelector;

export default configureStore({ reducer });
