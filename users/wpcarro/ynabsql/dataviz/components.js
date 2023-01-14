const colors = { 
    red: 'rgb(255, 45, 70)',
    green: 'rgb(75, 192, 35)',
    white: 'rgb(249, 246, 238)',
    blue: 'rgb(137, 207, 240)',
    fadedBlue: 'rgb(137, 207, 240, 0.25)',
    purple: 'rgb(203, 195, 227)',
    brown: 'rgb(205, 127, 50)',
    black: 'rgb(53, 57, 53)',
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
  
function getWeek(x) {
    const dowOffset = 0;
    var newYear = new Date(x.getFullYear(), 0, 1);
    var day = newYear.getDay() - dowOffset; //the day of week the year begins on
    day = (day >= 0 ? day : day + 7);
    var daynum = Math.floor((x.getTime() - newYear.getTime() -
        (x.getTimezoneOffset() - newYear.getTimezoneOffset()) * 60000) / 86400000) + 1;
    var weeknum;
    //if the year starts before the middle of a week
    if (day < 4) {
        weeknum = Math.floor((daynum + day - 1) / 7) + 1;
        if (weeknum > 52) {
            nYear = new Date(x.getFullYear() + 1, 0, 1);
            nday = nYear.getDay() - dowOffset;
            nday = nday >= 0 ? nday : nday + 7;
            /*if the next year starts before the middle of
              the week, it is week #1 of that year*/
            weeknum = nday < 4 ? 1 : 53;
        }
    }
    else {
        weeknum = Math.floor((daynum + day - 1) / 7);
    }
    return weeknum;
}
  
function dollars(n, sensitive) {
    if (sensitive) {
        const order = magnitude(n);
        // Shortcut to avoid writing comma-insertion logic v0v.
        if (n === 0) {
            return '$X.XX';
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
  
const categories = data.data.transactions.reduce((xs, x) => { 
    if (!(x.Category in xs)) {
        xs[x.Category] = [];
    }
    xs[x.Category].push(x);
    return xs;
}, {});
  
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
 * Generic line chart parameterized by:
 * - datasets: forwarded to chart.js library
 * - x: string label for x-axis
 * - y: string label for y-axis
 */
class GenLineChart extends React.Component {
    constructor(props) {
        super(props);
        this.chart = null;
        // Generate a 1/1M random ID.
        this.id = btoa(Math.floor(Math.random() * 1e9));
    }

    componentDidUpdate(prevProps, prevState) {
        if (this.props.datasets != prevProps.datasets) {
            this.chart.data.datasets = this.props.datasets;
            this.chart.update();
        }
    }

    componentDidMount() {
        const mount = document.getElementById(this.id);
        this.chart = new Chart(mount, {
            type: 'line',
            data: {
                datasets: this.props.datasets,
            },
            options: {
                scales: {
                    x: {
                        type: 'time',
                        title: {
                            display: true,
                            text: this.props.x,
                        },
                    },
                    y: {
                        title: {
                            display: true,
                            text: this.props.y
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

class DonutChart extends React.Component {
    constructor(props) {
        super(props);
        this.chart = null;
        // Generate a 1/1M random ID.
        this.id = btoa(Math.floor(Math.random() * 1e9));
    }

    componentDidUpdate(prevProps, prevState) {
        if (this.props.datasets != prevProps.datasets) {
            this.chart.data.datasets = this.props.datasets;
            this.chart.update();
        }
    }

    componentDidMount() {
        const mount = document.getElementById(this.id);
        this.chart = new Chart(mount, {
            type: 'doughnut',
            data: {
                labels: this.props.labels,
                datasets: this.props.datasets,
            },
            options: {
                resonsive: true,
            },
        });
    }

    render() {
        return <canvas id={this.id}></canvas>;
    }
}

class StackedHistogram extends React.Component {
    constructor(props) {
        super(props);
        this.chart = null;
        // Generate a 1/1M random ID.
        this.id = btoa(Math.floor(Math.random() * 1e9));
    }

    componentDidUpdate(prevProps, prevState) {
        if (this.props.datasets != prevProps.datasets) {
            this.chart.data.datasets = this.props.datasets;
            this.chart.update();
        }
    }

    componentDidMount() {
        const mount = document.getElementById(this.id);
        this.chart = new Chart(mount, {
            type: 'bar',
            data: {
                labels: this.props.labels,
                datasets: this.props.datasets,
            },
            options: {
                scales: {
                    x: {
                        stacked: true,
                    },
                    y: {
                        stacked: true,
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
class SavingsRateLineChart extends React.Component {
    constructor(props) {
        super(props);
        this.chart = null;
        // Generate a 1/1M random ID.
        this.id = btoa(Math.floor(Math.random() * 1e9));
    }

    static getRevenue(transactions) {
        // Bucket revenues into months.
        return transactions.reduce((acc, x) => {
            const month = x.Date.getMonth();
            acc[month] += x.Inflow;
            return acc;
        }, new Array(12).fill(0));
    }

    static getExpenses(transactions) {
        // Bucket revenues into months.
        return transactions.reduce((acc, x) => {
            const month = x.Date.getMonth();
            acc[month] += x.Outflow;
            return acc;
        }, new Array(12).fill(0));
    }

    componentDidMount() {
        const mount = document.getElementById(this.id);
        const revenue = SavingsRateLineChart.getRevenue(this.props.transactions);
        const expenses = SavingsRateLineChart.getExpenses(this.props.transactions);

        this.chart = new Chart(mount, {
            type: 'line',
            data: {
                datasets: [
                    {
                        label: 'actual savings (by month)',
                        data: new Array(12).fill(null).map((_, i) => ({
                            x: i,
                            y: (revenue[i] - expenses[i]) / revenue[i],
                        })),
                        cubicInterpolationMode: 'monotone',
                        tension: 0.4,
                        borderColor: colors.fadedBlue,
                        backgroundColor: colors.fadedBlue,
                    },
                    {
                        label: 'actual savings (overall)',
                        data: new Array(12).fill(null).map((_, i) => ({
                            x: i,
                            y: this.props.rate,
                        })),
                        cubicInterpolationMode: 'monotone',
                        tension: 0.4,
                        borderColor: colors.blue,
                        backgroundColor: colors.blue,
                    },
                    // 0% marker (out of debt)
                    {
                        label: 'beginner (0%)',
                        data: new Array(12).fill(null).map((x, i) => ({
                            x: i,
                            y: 0.00,
                        })),
                        cubicInterpolationMode: 'monotone',
                        tension: 0.4,
                        borderColor: colors.white,
                        backgroundColor: colors.white,
                    },
                    // 25% marker (quarter "Washington" club)
                    {
                        label: 'healthy (25%)',
                        data: new Array(12).fill(null).map((x, i) => ({
                            x: i,
                            y: 0.25,
                        })),
                        cubicInterpolationMode: 'monotone',
                        tension: 0.4,
                        borderColor: colors.purple,
                        backgroundColor: colors.purple,
                    },
                    // 50% marker (1/2-dollar "Kennedy" club)
                    {
                        label: 'rich (50%)',
                        data: new Array(12).fill(null).map((x, i) => ({
                            x: i,
                            y: 0.50,
                        })),
                        cubicInterpolationMode: 'monotone',
                        tension: 0.4,
                        borderColor: colors.brown,
                        backgroundColor: colors.brown,
                    },
                    // 75% marker
                    {
                        label: 'wealthy (75%)',
                        data: new Array(12).fill(null).map((x, i) => ({
                            x: i,
                            y: 0.75,
                        })),
                        cubicInterpolationMode: 'monotone',
                        tension: 0.4,
                        borderColor: colors.black,
                        backgroundColor: colors.black,
                    },
                ],
                labels: months,
            },
            options: {
                scales: {
                    y: {
                        max: 1.0,
                        min: -1.0,
                        title: {
                            display: true,
                            text: 'Savings Rate (%)'
                        },
                    },
                },
            },
        });
    }

    componentDidUpdate(prevProps, prevState) {
        // Bucket revenues into months.
        const revenue = SavingsRateLineChart.getRevenue(this.props.transactions);
        const expenses = SavingsRateLineChart.getExpenses(this.props.transactions);

        this.chart.data.datasets[0].data = new Array(12).fill(null).map((_, i) => ({
            x: i,
            y: (revenue[i] - expenses[i]) / revenue[i],
        }));
        this.chart.update();
    }

    render() {
        return <canvas id={this.id}></canvas>;
    }
}

class App extends React.Component {
    constructor(props) {
        super(props);
        const query = 'Account:/checking/ after:"01/01/2022" before:"01/01/2023"';
        const savingsView = 'after:"01/01/2022" before:"01/01/2023"';
        const inflowQuery = 'Account:/checking/';
        const outflowQuery = 'Account:/checking/ -Category:/(stocks|crypto)/';

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
            budget: [
                { 
                    label: 'Flexible', 
                    children: [
                        { label: 'groceries', savings: false, monthly: 400.00 },
                        { label: 'eating out', savings: false, monthly: 200.00 },
                        { label: 'alcohol', savings: false, monthly: 200.00 },
                        { label: 'household items', savings: false, monthly: 50.00 },
                        { label: 'toiletries', savings: false, monthly: 200.00 / 12 },
                        { label: 'haircuts', savings: false, monthly: 400.00 / 12 },
                        { label: 'gasoline', savings: false, monthly: 100.00 },
                        { label: 'parking', savings: false, monthly: 10.00 },
                        { label: 'ride services', savings: false, monthly: 50.00 },
                        { label: 'LMNT', savings: false, monthly: 45.00 },
                        { label: 'books', savings: false, monthly: 25.00 },
                        { label: 'vacation', savings: false, monthly: 4000.00 / 12 },
                        { label: 'reimbursement', savings: false, monthly: 0.00 },
                    ],
                },
                { 
                    label: 'Fixed', 
                    children: [
                        { label: 'rent', savings: false, monthly: 3100.00 },
                        { label: 'electric', savings: false, monthly: 50.00 },
                        { label: 'gas', savings: false, monthly: 30.00 },
                        { label: 'YNAB', savings: false, monthly: 100.00 / 12 },
                        { label: 'Robinhood Gold', savings: false, monthly: 5.00 },
                        { label: 'Spotify', savings: false, monthly: 10.00 },
                        { label: 'Surfline', savings: false, monthly: 100.00 / 12 },
                        { label: 'HBO Max', savings: false, monthly: 170.00 },
                        { label: 'Clear', savings: false, monthly: 179.00 },
                        { label: 'car insurance', savings: false, monthly: 100.00 },
                        { label: 'Making Sense', savings: false, monthly: 50.00 / 12 },
                        { label: 'internet', savings: false, monthly: 100.00 },
                        { label: 'tax return', savings: false, monthly: 200.00 / 12 },
                    ],
                },
                {
                    label: 'Rainy Day (dont touch)',
                    children: [
                        { label: 'emergency fund', savings: false, monthly: 0.00 },
                        { label: 'check-ups', savings: false, monthly: 7.50 },
                        { label: 'car maintenance', savings: false, monthly: 98.33 },
                    ],
                },
                {
                    label: 'Savings (dont touch)',
                    children: [
                        { label: 'stocks', savings: true, monthly: 4000.00 },
                        { label: 'crypto', savings: true, monthly: 736.00 },
                    ],
                },
                {
                    label: 'Gifts (dont touch)',
                    children: [
                        { label: 'birthdays', savings: false, monthly: 250.00 / 12 },
                        { label: 'Valentines Day', savings: false, monthly: 100.00 / 12 },
                        { label: 'Mothers Day', savings: false, monthly: 25.00 / 12 },
                        { label: 'Fathers Day', savings: false, monthly: 25.00 / 12 },
                        { label: 'Christmas', savings: false, monthly: 500.00 / 12 },
                    ],
                },
                {
                    label: 'Error Budget',
                    children: [
                        { label: 'stuff I forgot to budget for', savings: false, monthly: 0.00 },
                    ],
                },
            ],
            paycheck: 6000.00,
            view: 'budget',
            savingsView,
            inflowQuery,
            outflowQuery,
            inflowTransactions: select(inflowQuery, select(savingsView, data.data.transactions)),
            outflowTransactions: select(outflowQuery, select(savingsView, data.data.transactions)),
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
                    savingsView={this.state.savingsView}
                    inflowQuery={this.state.inflowQuery}
                    outflowQuery={this.state.outflowQuery}
                    inflowTransactions={this.state.inflowTransactions}
                    outflowTransactions={this.state.outflowTransactions}
                    onFilterInflow={() => this.setState({
                        inflowTransactions: select(this.state.inflowQuery, select(this.state.savingsView, data.data.transactions)),
                    })}
                    onFilterOutflow={() => this.setState({
                        outflowTransactions: select(this.state.outflowQuery, select(this.state.savingsView, data.data.transactions)),
                    })}
                    onFilterSavingsView={() => this.setState({
                        inflowTransactions: select(this.state.inflowQuery, select(this.state.savingsView, data.data.transactions)),
                        outflowTransactions: select(this.state.outflowQuery, select(this.state.savingsView, data.data.transactions)),
                    })}
                    onSavingsViewChange={x => this.setState({ savingsView: x })}
                    onInflowQueryChange={x => this.setState({ inflowQuery: x })}
                    onOutflowQueryChange={x => this.setState({ outflowQuery: x })}
                />
            );
        } else if (this.state.view === 'budget') {
            // Planned expenses:
            // - minus planned assignment to emergency fund (not an expense)
            // - minus planned spend to investments (e.g. stocks, crypto)
            const budgetedSpend = this.state.budget.reduce((acc, x) => acc + x.children.filter(x => x.savings).reduce((acc, x) => acc + x.monthly, 0), 0);

            view = (
                <div>
                    <fieldset>
                        <legend>Details</legend>
                        <div className="form-group">
                            <label htmlFor="paycheck">Paycheck</label>
                            <input name="paycheck" type="text" />
                        </div>
                        <div className="form-group">
                            <label htmlFor="savings-rate">Savings Rate</label>
                            <input name="savings-rate" type="text" />
                        </div>
                    </fieldset>
                    <ul>
                        <li>Available Spend: {dollars(this.state.paycheck * 2, this.state.sensitive)}</li>
                        <li>Target Spend: {dollars(this.state.paycheck, this.state.sensitive)}</li>
                        <li>Budgeted Spend (minus savings): {dollars(budgetedSpend, this.state.sensitive)}</li>
                        <li>Emergency Fund Size (recommended): {dollars(budgetedSpend * 3, this.state.sensitive)}</li>
                    </ul>
                    <div style={{ width: '30em' }}>
                        <DonutChart labels={this.state.budget.map(x => x.label)} datasets={[
                            {
                                label: 'Categories',
                                data: this.state.budget.map(x => x.children.reduce((acc, y) => acc + y.monthly, 0)),
                            }
                        ]} />
                    </div>
                    <ul>
                        {this.state.budget.map(x => (
                            <li>
                                <div>{x.label} - {dollars(x.children.reduce((acc, x) => acc + x.monthly, 0), this.state.sensitive)}</div>
                                <ul>{x.children.map(y => <li>{y.label} - {dollars(y.monthly, this.state.sensitive)}</li>)}</ul>
                            </li>
                        ))}
                    </ul>
                </div>
            );
        }

        return (
            <div className="container">
                <nav className="terminal-menu">
                    <ul>
                        <li><a href="#" onClick={() => this.setState({ view: 'query' })}>query</a></li>
                        <li><a href="#" onClick={() => this.setState({ view: 'savings' })}>savings</a></li>
                        <li><a href="#" onClick={() => this.setState({ view: 'budget' })}>budget</a></li>
                        <li><a href="#" onClick={() => this.setState({ sensitive: !this.state.sensitive })}>sensitive</a></li>
                    </ul>
                </nav>
                {view}
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
);

function classifyRate(x) {
    if (x < 0.25) {
        return 'needs improvement';
    }
    if (x < 0.50) {
        return 'healthy';
    }
    if (x < 0.75) {
        return 'rich';
    }
    if (x < 1.00) {
        return 'wealthy';
    }
}

const SavingsView = ({
    sensitive,
    savingsView,
    inflowQuery,
    outflowQuery,
    inflowTransactions,
    outflowTransactions,
    onSavingsViewChange,
    onInflowQueryChange,
    onOutflowQueryChange,
    onFilterInflow,
    onFilterOutflow,
    onFilterSavingsView,
}) => {
    const revenue = inflowTransactions.reduce((acc, x) => {
        acc[x.Date.getMonth()] += x.Inflow;
        return acc;
    }, new Array(12).fill(0));

    const inflow = inflowTransactions.reduce((acc, x) => acc + x.Inflow, 0);
    const outflow = outflowTransactions.reduce((acc, x) => acc + x.Outflow, 0);

    const delta25Sum = new Array(12).fill(0);
    for (let i = 1; i < 12; i += 1) {
        delta25Sum[i] = delta25Sum[i - 1] + revenue[i] * 0.25;
    }

    const delta50Sum = new Array(12).fill(0);
    for (let i = 1; i < 12; i += 1) {
        delta50Sum[i] = delta50Sum[i - 1] + revenue[i] * 0.5;
    }

    const delta75Sum = new Array(12).fill(0);
    for (let i = 1; i < 12; i += 1) {
        delta75Sum[i] = delta75Sum[i - 1] + revenue[i] * 0.75;
    }

    return (
        <section>
            <fieldset>
                <legend>Filtering</legend>
                <div className="form-group">
                    <label htmlFor="savings-view">Savings View</label>
                    <input name="savings-view" type="text" placeholder="Savings View..." value={savingsView}
                        onChange={e => onSavingsViewChange(e.target.value)} />
                    <button className="btn btn-default" onClick={() => onFilterSavingsView()}>Apply</button>
                </div>
                <div className="form-group">
                    <label htmlFor="inflow-query">Inflow Query</label>
                    <input name="inflow-query" type="text" placeholder="Inflow query..." value={inflowQuery}
                        onChange={e => onInflowQueryChange(e.target.value)} />
                    <button className="btn btn-default" onClick={() => onFilterInflow()}>Filter</button>
                </div>
                <div className="form-group">
                    <label htmlFor="outflow-query">Outflow Query</label>
                    <input name="outflow-query" type="text" placeholder="Outflow query..." value={outflowQuery}
                        onChange={e => onOutflowQueryChange(e.target.value)} />
                    <button className="btn btn-default" onClick={() => onFilterOutflow()}>Filter</button>
                </div>
            </fieldset>
            <ul>
                <li>inflow: {dollars(inflow, sensitive)}</li>
                <li>outflow: {dollars(outflow)}</li>
                <li>savings: {dollars(inflow - outflow)}</li>
                <li>rate: {parseFloat((inflow - outflow) / inflow * 100).toFixed(2)+"%"} ({classifyRate((inflow - outflow) / inflow)})</li>
            </ul>
            <SavingsRateLineChart rate={(inflow - outflow) / inflow} transactions={outflowTransactions} />
            {/* O($1,000) */}
            <StackedHistogram labels={months} datasets={Object.keys(categories).map(k => ({
                label: k,
                data: categories[k].reduce((acc, x) => {
                    acc[x.Date.getMonth()] += x.Outflow;
                    return acc;
                }, new Array(12).fill(0)).map((x, i) => ({
                    x: i,
                    y: x,
                }))
            }))} />
            {/* O($100) */}
            <StackedHistogram labels={months} datasets={Object.keys(categories).map(k => ({
                label: k,
                data: categories[k].reduce((acc, x) => {
                    acc[x.Date.getMonth()] += x.Outflow;
                    return acc;
                }, new Array(12).fill(0)).map((x, i) => ({
                    x: i,
                    y: x,
                }))
            }))} />
            {/* O($10) */}
            <StackedHistogram labels={months} datasets={Object.keys(categories).map(k => ({
                label: k,
                data: categories[k].reduce((acc, x) => {
                    acc[x.Date.getMonth()] += x.Outflow;
                    return acc;
                }, new Array(12).fill(0)).map((x, i) => ({
                    x: i,
                    y: x,
                }))
            }))} />
            {/* <GenLineChart x="X" y="Y" datasets={[
                {
                    label: '25%',
                    data: delta25Sum.map((x, i) => ({
                        x: i,
                        y: x,
                    })),
                },
                {
                    label: '50%',
                    data: delta50Sum.map((x, i) => ({
                        x: i,
                        y: x,
                    })),
                },
                {
                    label: '75%',
                    data: delta75Sum.map((x, i) => ({
                        x: i,
                        y: x,
                    })),
                },
            ]} /> */}
            <hr />
            <table>
                <thead>
                    <tr>
                        <th>Month</th>
                        <th>Delta (25%)</th>
                        <th>Delta (50%)</th>
                        <th>Delta (75%)</th>
                    </tr>
                </thead>
                <tbody>
                {months.map((x, i) => (
                    <tr key={i}>
                        <td>{x}</td>
                        <td>{dollars(delta25Sum[i], sensitive)}</td>
                        <td>{dollars(delta50Sum[i], sensitive)}</td>
                        <td>{dollars(delta75Sum[i], sensitive)}</td>
                    </tr>
                ))}
                </tbody>
            </table>
        </section>
    );
};

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
);

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
            <label htmlFor="paycheck">Paycheck</label>
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
