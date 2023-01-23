import React from 'react';
import ReactDOM from 'react-dom/client';
import Chart from 'chart.js/auto';
import 'chartjs-adapter-date-fns';

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
    const newYear = new Dte(x.getFullYear(), 0, 1);
    let day = newYear.getDay() - dowOffset; //the day of week the year begins on
    day = (day >= 0 ? day : day + 7);
    const daynum = Math.floor((x.getTime() - newYear.getTime() -
        (x.getTimezoneOffset() - newYear.getTimezoneOffset()) * 60000) / 86400000) + 1;
    let weeknum;

    //if the year starts before the middle of a week
    if (day < 4) {
        weeknum = Math.floor((daynum + day - 1) / 7) + 1;
        if (weeknum > 52) {
            const nYear = new Date(x.getFullYear() + 1, 0, 1);
            let nday = nYear.getDay() - dowOffset;
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

// Convert a sorting expressions (e.g. "Outflow DESC; Date ASC; Category ASC")
// into a function that can be passed to Array.prototype.sort.
function compileSort(expr) {
    return expr.split(/\s*;\s*/).reverse().reduce((acc, x) => {
        const [k, dir] = x.split(/\s+/);
        if (dir === 'ASC') {
            return function(x, y) {
                if (x[k] > y[k]) { return 1; }
                if (x[k] < y[k]) { return -1; }
                else { return acc(x, y); }
            };
        }
        if (dir === 'DESC') {
            return function(x, y) {
                if (x[k] > y[k]) { return -1; }
                if (x[k] < y[k]) { return 1; }
                else { return acc(x, y); }
            };
        }
        else {
            throw new Error(`Sort direction not supported, ${dir}, must be either "ASC" or "DESC"`);
        }
    }, function(x, y) { return 0; })
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

function parseCSV(csv) {
  var lines=csv.split("\n");
  var result = [];

  // Strip the surrounding 2x-quotes from the header.
  //
  // NOTE: If your columns contain commas in their values, you'll need
  // to deal with those before doing the next step 
  var headers = lines[0].split(",").map(x => x.slice(1, -1));

  for(var i = 1; i < lines.length; i += 1) {
      var obj = {};
      var currentline=lines[i].split(",");

      for(var j = 0; j <  headers.length; j += 1) { 
        obj[headers[j]] = currentline[j].slice(1, -1);
      }

      result.push(obj);
  }

  return result.map(x => ({
    ...x,
    Date: new Date(x.Date),
    Inflow: parseFloat(x.Inflow),
    Outflow: parseFloat(x.Outflow),
  }));
}


class UploadJSON extends React.Component {
    handleUpload(e) {
        let files = e.target.files;
        if (!files.length) {
            alert('No file selected!');
            return;
        }
        let file = files[0];
        let reader = new FileReader();
        reader.onload = (event) => {
            this.props.onUpload(parseCSV(event.target.result));
        };
        reader.readAsText(file);
    }
    render() {
        return <input onChange={e => this.handleUpload(e)} id="json-upload" type="file" accept="application/csv" />;
    }
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
                            title: function (x) {
                                return `$${x[0].raw.y} (${x[0].raw.metadata.Date.toLocaleDateString()})`;
                            },
                            label: function (x) {
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
        const query = 'Account:/checking/ (Inflow>1000 OR Outflow>1000)';
        const allTransactions = data.data.transactions;
        const savingsView = 'after:"01/01/2022"';
        const inflowQuery = 'Account:/checking/';
        const outflowQuery = 'Account:/checking/ -Category:/(stocks|crypto)/';

        // slx configuration
        const slxCaseSensitive = false;
        const slxPreferRegex = true;
        const slxDateKey = 'Date';

        this.state = {
            query,
            transactionsView: 'table',
            sensitive: false,
            allTransactions,
            slxCaseSensitive,
            slxPreferRegex,
            slxDateKey,
            filteredTransactions: select(query, allTransactions, {
                caseSensitive: slxCaseSensitive,
                preferRegex: slxPreferRegex,
                dateKey: slxDateKey,
            }),
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
                        { label: 'groceries - $200/mo', savings: false, monthly: 400.00 },
                        { label: 'eating out - $150/mo', savings: false, monthly: 200.00 },
                        { label: 'alcohol - $200/mo', savings: false, monthly: 200.00 },
                        { label: 'household items - $50/mo', savings: false, monthly: 50.00 },
                        { label: 'toiletries - $200/yr', savings: false, monthly: 200.00 / 12 },
                        { label: 'haircuts - $400/yr', savings: false, monthly: 400.00 / 12 },
                        { label: 'gasoline - $100/mo', savings: false, monthly: 100.00 },
                        { label: 'parking - $10/mo', savings: false, monthly: 10.00 },
                        { label: 'ride services - $25/mo', savings: false, monthly: 50.00 },
                        { label: 'LMNT - $45/mo', savings: false, monthly: 45.00 },
                        { label: 'books - $25/mo', savings: false, monthly: 25.00 },
                        { label: 'vacation - $4,000/yr', savings: false, monthly: 4000.00 / 12 },
                        { label: 'reimbursements - $5,000 balance', savings: false, monthly: 0.00 },
                    ],
                },
                {
                    label: 'Fixed',
                    children: [
                        { label: 'rent - $3,100/mo', savings: false, monthly: 3100.00 },
                        { label: 'electric - $50/mo', savings: false, monthly: 50.00 },
                        { label: 'SoCalGas - $30/mo', savings: false, monthly: 30.00 },
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
                        { label: 'stuff I forgot to budget for - $2,254/mo', savings: false, monthly: 0.00 },
                    ],
                },
            ],
            paycheck: 6000.00,
            view: 'query',
            savingsView,
            inflowQuery,
            outflowQuery,
            inflowTransactions: select(inflowQuery, select(savingsView, allTransactions, {
                caseSensitive: slxCaseSensitive,
                preferRegex: slxPreferRegex,
                dateKey: slxDateKey,
            }), {
                caseSensitive: slxCaseSensitive,
                preferRegex: slxPreferRegex,
                dateKey: slxDateKey,
            }),
            outflowTransactions: select(outflowQuery, select(savingsView, allTransactions, {
                caseSensitive: slxCaseSensitive,
                preferRegex: slxPreferRegex,
                dateKey: slxDateKey,
            }), {
                caseSensitive: slxCaseSensitive,
                preferRegex: slxPreferRegex,
                dateKey: slxDateKey,
            }),
            sortExpr: 'Date DESC; Outflow DESC',
        };
    }

    render() {
        const sum = this.state.filteredTransactions.reduce((acc, { Outflow }) => acc + Outflow, 0);
        const savedSum = Object.values(this.state.saved).reduce((acc, sum) => acc + sum, 0);

        let view = null;
        if (this.state.view === 'query') {
            view = (
                <QueryView
                    transactionsView={this.state.transactionsView}
                    sortExpr={this.state.sortExpr}
                    onSortExprChange={sortExpr => this.setState({ sortExpr })}
                    sensitive={this.state.sensitive}
                    query={this.state.query}
                    focus={this.state.focus}
                    allTransactions={this.state.allTransactions}
                    transactions={this.state.filteredTransactions}
                    saved={this.state.saved}
                    setState={this.setState.bind(this)}
                    slxConfig={{
                        caseSensitive: this.state.slxCaseSensitive,
                        preferRegex: this.state.slxPreferRegex,
                        dateKey: this.state.slxDateKey,
                    }}
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
                        inflowTransactions: select(this.state.inflowQuery, select(this.state.savingsView, this.state.allTransactions, {
                            caseSensitive: this.state.slxCaseSensitive,
                            preferRegex: this.state.slxPreferRegex,
                            dateKey: this.state.slxDateKey,
                        }), {
                            caseSensitive: this.state.slxCaseSensitive,
                            preferRegex: this.state.slxPreferRegex,
                            dateKey: this.state.slxDateKey,
                        }),
                    })}
                    onFilterOutflow={() => this.setState({
                        outflowTransactions: select(this.state.outflowQuery, select(this.state.savingsView, this.state.allTransactions, {
                            caseSensitive: this.state.slxCaseSensitive,
                            preferRegex: this.state.slxPreferRegex,
                            dateKey: this.state.slxDateKey,
                        }), {
                            caseSensitive: this.state.slxCaseSensitive,
                            preferRegex: this.state.slxPreferRegex,
                            dateKey: this.state.slxDateKey,
                        }),
                    })}
                    onFilterSavingsView={() => this.setState({
                        inflowTransactions: select(this.state.inflowQuery, select(this.state.savingsView, this.state.allTransactions, {
                            caseSensitive: this.state.slxCaseSensitive,
                            preferRegex: this.state.slxPreferRegex,
                            dateKey: this.state.slxDateKey,
                        }), {
                            caseSensitive: this.state.slxCaseSensitive,
                            preferRegex: this.state.slxPreferRegex,
                            dateKey: this.state.slxDateKey,
                        }),
                        outflowTransactions: select(this.state.outflowQuery, select(this.state.savingsView, this.state.allTransactions, {
                            caseSensitive: this.state.slxCaseSensitive,
                            preferRegex: this.state.slxPreferRegex,
                            dateKey: this.state.slxDateKey,
                        }), {
                            caseSensitive: this.state.slxCaseSensitive,
                            preferRegex: this.state.slxPreferRegex,
                            dateKey: this.state.slxDateKey,
                        }),
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
                                <table>
                                    <thead>
                                        <tr>
                                            <th>category</th>
                                            <th>assigned (target)</th>
                                            <th>last year (actual)</th>
                                        </tr>
                                    </thead>
                                    <tbody>
                                        {x.children.map(y => (
                                            <tr>
                                                <td>{y.label}</td>
                                                <td>{dollars(y.monthly, this.state.sensitive)}</td>
                                                <td>{dollars((categories[y.label] || []).reduce((acc, x) => acc + x.Outflow, 0) / 12, this.state.sensitive)}</td>
                                            </tr>
                                        ))}
                                    </tbody>
                                </table>
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
                <UploadJSON onUpload={xs => this.setState({ allTnransactions: xs })} />
                {view}
            </div>
        );
    }
}

const QueryView = ({ sensitive, query, focus, allTransactions, transactions, saved, setState, slxConfig, sortExpr, transactionsView }) => (
    <div>
        <Query
            query={query}
            onChange={query => setState({
                query,
            })}
            onFilter={() => setState({
                filteredTransactions: select(query, allTransactions, slxConfig),
            })}
        />
        <fieldset>
            <legend>Sort</legend>
            <div className="form-group">
                <input type="text" value={sortExpr} onChange={e => setState({ sortExpr: e.target.value, })} />
            </div>
            <div className="form-group">
                <button className="btn btn-default" onClick={() => setState({
                    filteredTransactions: transactions.slice().sort(compileSort(sortExpr)),
                })}>Sort</button>
            </div>
        </fieldset>
        <hr />
        <ScatterChart transactions={transactions} />
        <hr />
        <Transactions
            transactionsView={transactionsView}
            sensitive={sensitive}
            transactions={transactions}
            onClick={x => setState({
                saved: { ...saved, [transactionKey(x)]: x.Outflow }
            })}
            onViewChange={transactionsView => setState({ transactionsView })}
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
                <li>rate: {parseFloat((inflow - outflow) / inflow * 100).toFixed(2) + "%"} ({classifyRate((inflow - outflow) / inflow)})</li>
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
                <tr style={{ backgroundColor: '#F0F8FF' }}>
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
    }, { 0.1: [], 0: [], 1: [], 10: [], 100: [], 1000: [] });

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
        </div>
        <div className="form-group">
            <button className="btn btn-default" onClick={() => onFilter()}>Filter</button>
        </div>
    </fieldset>
);

const tableHeaders = [
    'Account',
    'Category',
    'Date',
    'Inflow',
    'Outflow',
    'Payee',
    'Memo',
];

const Transactions = ({ sensitive, transactions, onSort, onClick, onViewChange, transactionsView }) => {
    let view = null;
    if (transactionsView === 'table') {
        view = (
            <table>
                <thead>
                    <tr>
                        {tableHeaders.map(x => <th>{x}</th>)}
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
    } 
    else if (transactionsView === 'csv') {
        view = (
            <code>{tableHeaders.join(',') + '\n' + transactions.map(x => tableHeaders.map(k => x[k]).join(',')).join("\n")}</code>
        );
    }
    else if (transactionsView === 'json') {
        view = (
            <code>{JSON.stringify(transactions)}</code>
        );
    }

    return (
        <div>
            <caption>Transactions</caption>
            <div className="btn-group">
                <button onClick={() => onViewChange('table')} className="btn btn-default btn-ghost">Table</button>
                <button onClick={() => onViewChange('csv')} className="btn btn-default btn-ghost">CSV</button>
                <button onClick={() => onViewChange('json')} className="btn btn-default btn-ghost">JSON</button>
            </div>
            {view}
        </div>
    );
}

const domContainer = document.querySelector('#mount');
const root = ReactDOM.createRoot(domContainer);

root.render(<App />);
