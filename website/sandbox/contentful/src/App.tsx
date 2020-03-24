import React, { useEffect } from "react";
import { BrowserRouter as Router, Switch, Route } from "react-router-dom";
import { useDispatch } from "react-redux";
import { actions, useTypedSelector } from "./store";
import { Link } from "react-router-dom";
import { getClient } from "./contentful";
import type { Book } from "./store";

const App: React.FC = () => {
  const dispatch = useDispatch();
  const { isLoading, books } = useTypedSelector(state => ({
    isLoading: state.isLoading,
    books: state.books,
  }));

  useEffect(() => {
    async function fetchData() {
      const entries = await getClient().getEntries();
      const books = entries.items.map(x => x.fields) as Book[];

      dispatch(actions.setBooks(books));
    }
    fetchData();
  }, []);

  return (
    <Router>
      <Switch>
        <Route exact path="/">
          <div className="container mx-auto">
            <h1 className="py-6 text-2xl">Books</h1>
            <ul>
              {books.map(book => (
                <li key={book.title} className="py-3">
                  <p><span className="font-bold pr-3">{book.title}</span><span className="text-gray-600">{book.author}</span></p>
                </li>
              ))}
            </ul>
          </div>
        </Route>
      </Switch>
    </Router>
  );
};

export default App;
