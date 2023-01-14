const colors = {
  red: 'rgb(255, 45, 70)',
  green: 'rgb(75, 192, 35)',
};

const months = [
    'January',
    'February',
    'March',
    'April',
    'May',
    'June',
    'July',
    'August',
    'September',
    'October',
    'November',
    'December',
];

function dollars(n, sensitive) {
    if (sensitive) {
        const order = magnitude(n);
        // Shortcut to avoid writing comma-insertion logic v0v.
        if (n === 0) {
            return '$0.00';
        }
        if (order <= 0) {
            return '$X.XX';
        }
        if (order === 1) {
            return '$XX.XX';
        }
        if (order === 2) {
            return '$XXX.XX';
        }
        if (order === 3) {
            return '$X,XXX.XX';
        }
        if (order === 4) {
            return '$XX,XXX.XX';
        }
        if (order === 4) {
            return '$XX,XXX.XX';
        }
        if (order === 5) {
            return '$XXX,XXX.XX';
        }
        // Coming soon! :P
        if (order === 6) {
            return '$X,XXX,XXX.XX';
        }
        if (order === 7) {
            return '$XX,XXX,XXX.XX';
        }
        if (order === 8) {
            return '$XXX,XXX,XXX.XX';
        }
        // Unsupported
        else {
            return '$???.??';
        }
    }
    return usd.format(n);
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

/**
 * Return the Order of Magnitude of some value, x.
 */
function magnitude(x) {
    return Math.floor(Math.log(x) / Math.LN10 + 0.000000001);
}

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

class ScatterChart extends React.Component {
    constructor(props) {
        super(props);
        this.chart = null;
        // Generate a 1/1M random ID.
        this.id = btoa(Math.floor(Math.random() * 1e9));
    }
    componentDidUpdate(prevProps) {
        if (this.props.transactions !== prevProps.transactions) {
            this.chart.data.datasets[0].data = this.props.transactions.filter(x => x.Inflow > 0).map(x => ({
                x: x.Date,
                y: x.Inflow,
                metadata: x,
            }));
            this.chart.data.datasets[1].data = this.props.transactions.filter(x => x.Outflow > 0).map(x => ({
                x: x.Date,
                y: x.Outflow,
                metadata: x,
            }));
            this.chart.update();
        }
    }
    componentDidMount() {
        const mount = document.getElementById(this.id);
        this.chart = new Chart(mount, {
          type: 'scatter',
          data: {
            datasets: [
              {
                label: 'Revenue',
                data: this.props.transactions.filter(x => x.Inflow > 0).map(x => ({
                  x: x.Date,
                  y: x.Inflow,
                  metadata: x,
                })),
                backgroundColor: colors.green,
              },
              {
                label: 'Expenses',
                data: this.props.transactions.filter(x => x.Outflow).map(x => ({
                  x: x.Date,
                  y: x.Outflow,
                  metadata: x,
                })),
                backgroundColor: colors.red,
              },
            ],
          },
          options: {
            scales: {
              x: {
                type: 'time',
                title: {
                  display: true,
                  text: 'Date',
                },
              },
              y: {
                title: {
                  display: true,
                  text: 'Amount ($USD)'
                },
              },
            },
            plugins: {
              tooltip: {
                callbacks: {
                  title: function(x) {
                    return `$${x[0].raw.y} (${x[0].raw.metadata.Date.toLocaleDateString()})`;
                  },
                  label: function(x) {
                    const { Category, Payee, Memo } = x.raw.metadata;
                    return `${Payee} - ${Category} (${Memo})`;
                  },
                },
              },
            },
          },
        });
    }
    render() {
        return <canvas id={this.id}></canvas>;
    }
}

/**
 * Display the "Actual Savings Rate" (bucketed by month) as a line chart with
 * the "Expected Savings Rate" overlay.
 */
class LineChart extends React.Component {
    constructor(props) {
        super(props);
        this.chart = null;

        // Bucket revenues into months.
        this.revenue = this.props.transactions.reduce((acc, x) => {
            const month = x.Date.getMonth();
            acc[month] += x.Inflow;
            return acc;
        }, new Array(12).fill(0));

        // Bucket profits into months.
        this.buckets = props.transactions.reduce((acc, x) => {
            const month = x.Date.getMonth();
            acc[month] += x.Inflow;
            acc[month] -= x.Outflow;
            return acc;
        }, new Array(12).fill(0));

        // Generate a 1/1M random ID.
        this.id = btoa(Math.floor(Math.random() * 1e9));
    }

    componentDidUpdate(prevProps, prevState) {
        this.revenue = this.props.transactions.reduce((acc, x) => {
            const month = x.Date.getMonth();
            acc[month] += x.Inflow;
            return acc;
        }, new Array(12).fill(0));

        this.buckets = this.props.transactions.reduce((acc, x) => {
            const month = x.Date.getMonth();
            acc[month] += x.Inflow;
            acc[month] -= x.Outflow;
            return acc;
        }, new Array(12).fill(0));

        this.chart.data.datasets[0].data = this.buckets.map((x, i) => ({
            x: i,
            y: x,
        }));
        this.chart.update();
    }

    componentDidMount() {
        const mount = document.getElementById(this.id);
        this.chart = new Chart(mount, {
            type: 'line',
            data: {
                datasets: [
                    {
                        label: 'Actual Savings Rate',
                        data: this.buckets.map((x, i) => ({
                            x: i,
                            y: x,
                        })),
                        cubicInterpolationMode: 'monotone',
                        tension: 0.4,
                        borderColor: colors.red,
                        backgroundColor: colors.red,
                    },
                    {
                        label: 'Expected Savings Rate',
                        data: this.revenue.map((x, i) => ({
                            x: i,
                            y: x / 2,
                        })),
                        cubicInterpolationMode: 'monotone',
                        tension: 0.4,
                        borderColor: colors.green,
                        backgroundColor: colors.green,
                    },
                ],
            },
            options: {
                scales: {
                    x: {
                        type: 'time',
                        title: {
                            display: true,
                            text: 'Month',
                        },
                    },
                    y: {
                        title: {
                            display: true,
                            text: 'Amount ($USD)'
                        },
                    },
                },
            },
        });
    }

    render() {
        return <canvas id={this.id}></canvas>;
    }
}

class App extends React.Component {
    constructor(props) {
        super(props);
        const query = 'Account:/checking/ after:"01/01/2022" before:"01/01/2023"';

        this.state = {
            query,
            sensitive: false,
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
            view: 'query',
        };
    }

    render() {
        const sum = this.state.transactions.reduce((acc, { Outflow }) => acc + Outflow, 0);
        const savedSum = Object.values(this.state.saved).reduce((acc, sum) => acc + sum, 0);

        let view = null;
        if (this.state.view === 'query') {
            view = (
                <QueryView
                    sensitive={this.state.sensitive}
                    query={this.state.query}
                    focus={this.state.focus}
                    transactions={this.state.transactions}
                    saved={this.state.saved}
                    setState={this.setState.bind(this)}
                />
            );
        } else if (this.state.view === 'savings') {
            view = (
                <SavingsView
                    sensitive={this.state.sensitive}
                    transactions={this.state.transactions}
                />
            );
        }

        return (
            <div className="container">
                {view}
                <footer>
                    <ul>
                        <li><a href="#" onClick={() => this.setState({ view: 'query' })}>query</a></li>
                        <li><a href="#" onClick={() => this.setState({ view: 'savings' })}>savings</a></li>
                    </ul>
                    <fieldset>
                        <label for="sensitive">Sensitive</label>
                        <input type="checkbox" name="sensitive" onChange={e => this.setState({ sensitive: e.target.checked })} />
                    </fieldset>
                </footer>
            </div>
        );
    }
}

const QueryView = ({ sensitive, query, focus, transactions, saved, setState }) => (
    <div>
        <Query
            query={query}
            onChange={query => setState({
                query,
            })}
            onFilter={() => setState({
                transactions: select(query, data.data.transactions),
            })}
        />
        <hr />
        <ScatterChart transactions={transactions} />
        <hr />
        <Transactions
            sensitive={sensitive}
            transactions={sortTransactions(transactions)}
            onClick={x => setState({
                saved: { ...saved, [transactionKey(x)]: x.Outflow }
            })}
        />
    </div>
)

const SavingsView = ({ sensitive, transactions }) => (
    <div>
        <SavingsRate
            sensitive={sensitive}
            transactions={transactions} />
    </div>
);

const Calculator = ({ sensitive, saved, onClear }) => (
    <div>
        <ul>
            {Object.keys(saved).map(k => (
                <li key={k}>
                    {dollars(saved[k], sensitive)} {k}
                </li>
            ))}
        </ul>
        <p>{dollars(savedSum, sensitive)}</p>
        <button className="btn btn-default" onClick={() => onClear()}>clear</button>
    </div>
)

const SavingsRate = ({ sensitive, transactions }) => {
    const revenue = transactions.reduce((acc, x) => {
        acc[x.Date.getMonth()] += x.Inflow;
        return acc;
    }, new Array(12).fill(0));

    const profit = transactions.reduce((acc, x) => {
        acc[x.Date.getMonth()] += x.Inflow - x.Outflow;
        return acc;
    }, new Array(12).fill(0));

    const delta = profit.map((x, i) => revenue[i] / 2 - x);

    const deltaSum = new Array(12).fill(delta[0]);
    for (let i = 1; i < 12; i += 1) {
        deltaSum[i] = deltaSum[i - 1] + delta[i];
    }

    return (
        <section>
            <LineChart transactions={transactions} />
            <hr />
            <table>
                <thead>
                    <tr><th>Month</th><th>Delta</th><th>Delta (sum)</th></tr>
                </thead>
                <tbody>
                {months.map((x, i) => (
                    <tr>
                        <td>{x}</td>
                        <td>{dollars(delta[i], sensitive)}</td>
                        <td>{dollars(deltaSum[i], sensitive)}</td>
                    </tr>
                ))}
                </tbody>
            </table>
        </section>
    )
};

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
        const order = magnitude(x.Outflow);
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
                    <tr onClick={() => onFocus(1000)}><td>Σ Θ($1,000)</td><td>{dollars(buckets[1000].reduce((acc, x) => acc + x.Outflow, 0), sensitive)}</td></tr>
                    {(focus[1000]) && <Magnitable sensitive={sensitive} label="$1,000" transactions={buckets[1000]} />}
                    <tr onClick={() => onFocus(100)}><td>Σ Θ($100)</td><td>{dollars(buckets[100].reduce((acc, x, sensitive) => acc + x.Outflow, 0), sensitive)}</td></tr>
                    {(focus[100]) && <Magnitable sensitive={sensitive} label="$100" transactions={buckets[100]} />}
                    <tr onClick={() => onFocus(10)}><td>Σ Θ($10)</td><td>{dollars(buckets[10].reduce((acc, x) => acc + x.Outflow, 0), sensitive)}</td></tr>
                    {(focus[10]) && <Magnitable sensitive={sensitive} label="$10" transactions={buckets[10]} />}
                    <tr onClick={() => onFocus(1)}><td>Σ Θ($1)</td><td>{dollars(buckets[1].reduce((acc, x) => acc + x.Outflow, 0), sensitive)}</td></tr>
                    {(focus[1]) && <Magnitable sensitive={sensitive} label="$1.00" transactions={buckets[1]} />}
                    <tr onClick={() => onFocus(0.1)}><td>Σ Θ($0.10)</td><td>{dollars(buckets[0.1].reduce((acc, x) => acc + x.Outflow, 0), sensitive)}</td></tr>
                    {(focus[0.1]) && <Magnitable sensitive={sensitive} label="$0.10" transactions={buckets[0.1]} />}
                    <tr><td>average</td><td>{dollars(sum / transactions.length, sensitive)}</td></tr>
                    <tr><td>count</td><td>{transactions.length}</td></tr>
                </tbody>
            </table>
        </div>
    );
};

const Query = ({ query, onChange, onFilter }) => (
    <fieldset>
        <legend>Query</legend>
        <div className="form-group">
            <input name="query" type="text" value={query} onChange={e => onChange(e.target.value)} />
            <div className="btn-group">
                <button className="btn btn-default" onClick={() => onFilter()}>Filter</button>
            </div>
        </div>
    </fieldset>
);

const Transactions = ({ sensitive, transactions, onClick }) => (
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

const domContainer = document.querySelector('#mount');
const root = ReactDOM.createRoot(domContainer);

root.render(<App />);
