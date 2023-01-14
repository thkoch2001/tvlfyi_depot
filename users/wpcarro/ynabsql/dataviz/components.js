const usd = new Intl.NumberFormat('en-US', {
  style: 'currency',
  currency: 'USD',
});

const categories = data.data.transactions.reduce((xs, x) => { xs[x.Category] = null; return xs; }, {});

function sortTransactions(transactions) {
    return [...transactions].sort((x, y) => {
        if (x.Outflow < y.Outflow) {
            return 1;
        } else if (x.Outflow > y.Outflow) {
            return -1;
        } else {
            return 0;
        }
    });
}

function transactionKey(x) {
    const keys = [
        'Account',
        'Flag',
        'Date',
        'Payee',
        'Category',
        'Memo',
        'Outflow',
        'Inflow',
        'Cleared',
    ];
    return keys.map(k => x[k]).join('|');
}

class App extends React.Component {
    constructor(props) {
        super(props);
        const query = 'Account:/checking/ after:"01/01/2022" before:"01/01/2023"';

        this.state = {
            query,
            transactions: select(query, data.data.transactions),
            saved: {},
            focus: {
                1000: false,
                100: false,
                10: false,
                1: false,
                0.1: false,
            },
        };
    }

    render() {
        const sum = this.state.transactions.reduce((acc, { Outflow }) => acc + Outflow, 0);
        const savedSum = Object.values(this.state.saved).reduce((acc, sum) => acc + sum, 0);

        return (
            <div className="container">
                <select>
                    {Object.keys(categories).map(x => (
                        <option value={x} key={x}>{x}</option>
                    ))}
                </select>
                <Input 
                    query={this.state.query} 
                    onChange={query => this.setState({ 
                        query,
                    })}
                    onFilter={() => this.setState({
                        transactions: select(this.state.query, data.data.transactions),
                    })} 
                    onSave={() => this.setState({
                        saved: { ...this.state.saved, [this.state.query]: sum }
                    })}
                />
                <AggregateTable 
                    focus={this.state.focus} 
                    onFocus={(n) => this.setState({
                        focus: { ...this.state.focus, [n]: !this.state.focus[n] },
                    })}
                    transactions={this.state.transactions} 
                />
                <hr />
                <div>
                    <ul>
                        {Object.keys(this.state.saved).map(k => (
                            <li key={k}>
                                {usd.format(this.state.saved[k])} {k}
                            </li>
                        ))}
                    </ul>
                    <p>{usd.format(savedSum)}</p>
                    <button className="btn btn-default" onClick={() => this.setState({ saved: {} })}>clear</button>
                </div>
                <hr />
                <Table 
                    transactions={sortTransactions(this.state.transactions)} 
                    onClick={x => this.setState({
                        saved: { ...this.state.saved, [transactionKey(x)]: x.Outflow }
                    })}
                />
            </div>
        );
    }
}

/**
 * Table rendering information about transactions bucketed by its order of
 * magnitude.
 */
const Magnitable = ({ label, transactions }) => {
    const categories = transactions.reduce((acc, x) => {
        if (x.Category === '') {
            return acc;
        }
        if (!(x.Category in acc)) {
            acc[x.Category] = 0;
        }
        acc[x.Category] += x.Outflow;
        return acc;
    }, {});

    // Sort category keys by sum decreasing.
    const keys = [...Object.keys(categories)].sort((x, y) => {
        if (categories[x] < categories[y]) {
            return 1;
        } else if (categories[x] > categories[y]) {
            return -1;
        } else {
            return 0;
        }
    });

    return (
        <React.Fragment>
            {keys.map(k => (
                <tr style={{backgroundColor: '#F0F8FF'}}>
                    <td>{k}</td><td>{usd.format(categories[k])}</td>
                </tr>
            ))}
        </React.Fragment>
    );
};

/**
 * Calculates and renders various aggregates over an input list of transactions.
 */
const AggregateTable = ({ focus, onFocus, transactions }) => {
    const sum = transactions.reduce((acc, x) => acc + x.Outflow, 0);
    const buckets = transactions.reduce((acc, x) => {
        const order = Math.floor(Math.log(x.Outflow) / Math.LN10 + 0.000000001);
        const bucket = Math.pow(10, order);
        acc[bucket].push(x);
        return acc;
    }, {0.1: [], 0: [], 1: [], 10: [], 100: [], 1000: []});

    return (
        <div>
            <table>
                <caption>Aggregations</caption>
                <thead>
                    <tr>
                        <th>function</th>
                        <th>value</th>
                    </tr>
                </thead>
                <tbody>
                    <tr><td>sum</td><td>{usd.format(sum)}</td></tr>
                    <tr><td>per day</td><td>{usd.format(sum / 365)}</td></tr>
                    <tr><td>per week</td><td>{usd.format(sum / 52)}</td></tr>
                    <tr><td>per month</td><td>{usd.format(sum / 12)}</td></tr>
                    <tr onClick={() => onFocus(1000)}><td>Σ Θ($1,000)</td><td>{usd.format(buckets[1000].reduce((acc, x) => acc + x.Outflow, 0))}</td></tr>
                    {(focus[1000]) && <Magnitable label="$1,000" transactions={buckets[1000]} />}
                    <tr onClick={() => onFocus(100)}><td>Σ Θ($100)</td><td>{usd.format(buckets[100].reduce((acc, x) => acc + x.Outflow, 0))}</td></tr>
                    {(focus[100]) && <Magnitable label="$100" transactions={buckets[100]} />}
                    <tr onClick={() => onFocus(10)}><td>Σ Θ($10)</td><td>{usd.format(buckets[10].reduce((acc, x) => acc + x.Outflow, 0))}</td></tr>
                    {(focus[10]) && <Magnitable label="$10" transactions={buckets[10]} />}
                    <tr onClick={() => onFocus(1)}><td>Σ Θ($1)</td><td>{usd.format(buckets[1].reduce((acc, x) => acc + x.Outflow, 0))}</td></tr>
                    {(focus[1]) && <Magnitable label="$1.00" transactions={buckets[1]} />}
                    <tr onClick={() => onFocus(0.1)}><td>Σ Θ($0.10)</td><td>{usd.format(buckets[0.1].reduce((acc, x) => acc + x.Outflow, 0))}</td></tr>
                    {(focus[0.1]) && <Magnitable label="$0.10" transactions={buckets[0.1]} />}
                    <tr><td>average</td><td>{usd.format(sum / transactions.length)}</td></tr>
                    <tr><td>count</td><td>{transactions.length}</td></tr>
                </tbody>
            </table>
        </div>
    );
};

const Input = ({ query, onChange, onFilter, onSave }) => (
    <fieldset>
        <legend>Query</legend>
        <div className="form-group">
            <input name="query" type="text" value={query} onChange={e => onChange(e.target.value)} />
            <div className="btn-group">
                <button className="btn btn-default" onClick={() => onFilter()}>Filter</button>
                <button className="btn btn-default" onClick={() => onSave()}>Save</button>
            </div>
        </div>
    </fieldset>
);

const Table = ({ transactions, onClick }) => (
    <table>
        <caption>Transactions</caption>
        <thead>
            <tr>
                <th>Account</th>
                <th>Category</th>
                <th>Date</th>
                <th>Outflow</th>
                <th>Payee</th>
                <th>Memo</th>
            </tr>
        </thead>
        <tbody>
            {transactions.map(x => (
                <tr onClick={() => onClick(x)}>
                    <td>{x.Account}</td>
                    <td>{x.Category}</td>
                    <td>{x.Date.toLocaleDateString()}</td>
                    <td>{usd.format(x.Outflow)}</td>
                    <td>{x.Payee}</td>
                    <td>{x.Memo}</td>
                </tr>
            ))}
        </tbody>
    </table>
);

const domContainer = document.querySelector('#react-mount');
const root = ReactDOM.createRoot(domContainer);

root.render(<App />);