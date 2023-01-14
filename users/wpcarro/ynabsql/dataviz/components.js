function dollars(n, sensitive) {
    return sensitive ? '$XX.XX' : usd.format(n);
}

const usd = new Intl.NumberFormat('en-US', {
  style: 'currency',
  currency: 'USD',
});

const categories = data.data.transactions.reduce((xs, x) => { xs[x.Category] = null; return xs; }, {});

const queries = {
    housing: 'Category:/(rent|electric)/',
    food: 'Category:/(eating|alcohol|grocer)/',
    commute: 'Category:/(vacation|gasoline|parking|car maintenance)/',
};

function getSum(transactions) {
    return transactions.reduce((acc, x) => acc + x.Outflow, 0);
}

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

class LineChart extends React.Component {
    constructor(props) {
        super(props);
        this.chart = null;
    }

    componentDidMount() {
        const mount = document.getElementById('foo');
        this.chart = new Chart(mount, {
            type: 'line',
            data: {
                datasets: [],
            },
        });
    }

    render() {
        return (
            <canvas id="foo"></canvas>
        );
    }
}

class App extends React.Component {
    constructor(props) {
        super(props);
        const query = 'Account:/checking/ after:"01/01/2022" before:"01/01/2023"';

        this.state = {
            query,
            sensitive: true,
            transactions: select(query, data.data.transactions),
            saved: {},
            focus: {
                sum: false,
                1000: false,
                100: false,
                10: false,
                1: false,
                0.1: false,
            },
            paycheck: 6800.00,
        };
    }

    componentDidUpdate(prevProps, prevState) {
        if (this.state.transactions !== prevState.transactions) {
            chart.data.datasets[0].data = this.state.transactions.filter(x => x.Inflow > 0).map(x => ({
                x: x.Date,
                y: x.Inflow,
                metadata: x,
            }));
            chart.data.datasets[1].data = this.state.transactions.filter(x => x.Outflow > 0).map(x => ({
                x: x.Date,
                y: x.Outflow,
                metadata: x,
            }));
            chart.update();
        }
    }

    render() {
        const sum = this.state.transactions.reduce((acc, { Outflow }) => acc + Outflow, 0);
        const savedSum = Object.values(this.state.saved).reduce((acc, sum) => acc + sum, 0);

        return (
            <div className="container">
                <LineChart name="William" />
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
                <hr />
                {/* <Forecast
                    sensitive={this.state.sensitive}
                    paycheck={this.state.paycheck}
                    onPaycheck={paycheck => this.setState({ paycheck })} />
                <hr /> */}
                <AggregateTable 
                    sensitive={this.state.sensitive}
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
                                {dollars(this.state.saved[k], this.state.sensitive)} {k}
                            </li>
                        ))}
                    </ul>
                    <p>{dollars(savedSum, this.state.sensitive)}</p>
                    <button className="btn btn-default" onClick={() => this.setState({ saved: {} })}>clear</button>
                </div>
                <hr />
                <Table 
                    sensitive={this.state.sensitive}
                    transactions={sortTransactions(this.state.transactions)} 
                    onClick={x => this.setState({
                        saved: { ...this.state.saved, [transactionKey(x)]: x.Outflow }
                    })}
                />
            </div>
        );
    }
}

const Forecast = ({ sensitive, paycheck, onPaycheck }) => {
    const getModel = k => {
        const max = paycheck / 3;
        const lastYear = getSum(select(`Account:/checking/ after:"01/01/2022" before:"01/01/2023" ${queries[k]}`, data.data.transactions)) / 12;
        return {
            max,
            lastYear,
            surplus: max - lastYear,
        };
    };

    const housing = getModel('housing');
    const food = getModel('food');
    const commute = getModel('commute');

    return (
        <fieldset>
            <legend>Forecasting</legend>
            <label for="paycheck">Paycheck</label>
            <input
                name="paycheck" type="text" placeholder="Last paycheck ($USD)"
                value={paycheck}
                onChange={e => onPaycheck(parseFloat(e.target.value))} />
            <table>
                <thead>
                    <tr>
                        <th>category</th>
                        <th>max</th>
                        <th>last year</th>
                        <th>surplus</th>
                    </tr>
                </thead>
                <tbody>
                    <tr>
                        <td>Housing</td>
                        <td>{dollars(housing.max, sensitive)}</td>
                        <td>{dollars(housing.lastYear, sensitive)}</td>
                        <td>{dollars(housing.surplus, sensitive)}</td>
                    </tr>
                    <tr>
                        <td>Food</td>
                        <td>{dollars(food.max, sensitive)}</td>
                        <td>{dollars(food.lastYear, sensitive)}</td>
                        <td>{dollars(food.surplus, sensitive)}</td>
                    </tr>
                    <tr>
                        <td>Commute</td>
                        <td>{dollars(commute.max, sensitive)}</td>
                        <td>{dollars(commute.lastYear, sensitive)}</td>
                        <td>{dollars(commute.surplus, sensitive)}</td>
                    </tr>
                    <tr>
                        <td>Sum</td>
                        <td>-</td>
                        <td>-</td>
                        <td>{dollars(housing.surplus + food.surplus + commute.surplus, sensitive)}</td>
                    </tr>
                </tbody>
            </table>
        </fieldset>
    );
};

/**
 * Table rendering information about transactions bucketed by its order of
 * magnitude.
 */
const Magnitable = ({ sensitive, label, transactions }) => {
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
                    <td>{k}</td><td>{dollars(categories[k], sensitive)}</td>
                </tr>
            ))}
        </React.Fragment>
    );
};

/**
 * Calculates and renders various aggregates over an input list of transactions.
 */
const AggregateTable = ({ sensitive, focus, onFocus, transactions }) => {
    const net = transactions.reduce((acc, x) => acc + x.Inflow - x.Outflow, 0);
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
                    <tr><td>net</td><td>{dollars(net, sensitive)}</td></tr>
                    <tr onClick={() => onFocus('sum')}><td>sum</td><td>{dollars(sum, sensitive)}</td></tr>
                    {focus.sum && <Magnitable sensitive={sensitive} label="sum" transactions={transactions} />}
                    <tr><td>per day</td><td>{dollars(sum / 365, sensitive)}</td></tr>
                    <tr><td>per week</td><td>{dollars(sum / 52, sensitive)}</td></tr>
                    <tr><td>per month</td><td>{dollars(sum / 12, sensitive)}</td></tr>
                    <tr onClick={() => onFocus(1000)}><td>Σ Θ($1,000)</td><td>{dollars(buckets[1000].reduce((acc, x, sensitive) => acc + x.Outflow, 0))}</td></tr>
                    {(focus[1000]) && <Magnitable sensitive={sensitive} label="$1,000" transactions={buckets[1000]} />}
                    <tr onClick={() => onFocus(100)}><td>Σ Θ($100)</td><td>{dollars(buckets[100].reduce((acc, x, sensitive) => acc + x.Outflow, 0))}</td></tr>
                    {(focus[100]) && <Magnitable sensitive={sensitive} label="$100" transactions={buckets[100]} />}
                    <tr onClick={() => onFocus(10)}><td>Σ Θ($10)</td><td>{dollars(buckets[10].reduce((acc, x, sensitive) => acc + x.Outflow, 0))}</td></tr>
                    {(focus[10]) && <Magnitable sensitive={sensitive} label="$10" transactions={buckets[10]} />}
                    <tr onClick={() => onFocus(1)}><td>Σ Θ($1)</td><td>{dollars(buckets[1].reduce((acc, x, sensitive) => acc + x.Outflow, 0))}</td></tr>
                    {(focus[1]) && <Magnitable sensitive={sensitive} label="$1.00" transactions={buckets[1]} />}
                    <tr onClick={() => onFocus(0.1)}><td>Σ Θ($0.10)</td><td>{dollars(buckets[0.1].reduce((acc, x, sensitive) => acc + x.Outflow, 0))}</td></tr>
                    {(focus[0.1]) && <Magnitable sensitive={sensitive} label="$0.10" transactions={buckets[0.1]} />}
                    <tr><td>average</td><td>{dollars(sum / transactions.length, sensitive)}</td></tr>
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

const Table = ({ sensitive, transactions, onClick }) => (
    <table>
        <caption>Transactions</caption>
        <thead>
            <tr>
                <th>Account</th>
                <th>Category</th>
                <th>Date</th>
                <th>Inflow</th>
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
                    <td>{dollars(x.Inflow, sensitive)}</td>
                    <td>{dollars(x.Outflow, sensitive)}</td>
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