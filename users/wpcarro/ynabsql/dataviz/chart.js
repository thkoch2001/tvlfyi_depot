const colors = {
  red: 'rgb(255, 45, 70)',
  green: 'rgb(75, 192, 35)',
};

////////////////////////////////////////////////////////////////////////////////
// Main
////////////////////////////////////////////////////////////////////////////////

const mount = document.getElementById('mount');

const chart = new Chart(mount, {
  type: 'scatter',
  data: {
    datasets: [
      {
        label: 'Revenue',
        data: data.data.transactions.filter(x => x.Inflow > 0).map(x => ({
          x: x.Date,
          y: x.Inflow,
          metadata: x,
        })),
        backgroundColor: colors.green,
      },
      {
        label: 'Expenses',
        data: data.data.transactions.filter(x => x.Outflow).map(x => ({
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
