const colors = {
  red: 'rgb(255, 99, 132)',
  orange: 'rgb(255, 159, 64)',
  yellow: 'rgb(255, 205, 86)',
  green: 'rgb(75, 192, 192)',
  blue: 'rgb(54, 162, 235)',
  purple: 'rgb(153, 102, 255)',
  grey: 'rgb(201, 203, 207)'
};

function randomExpense() {
  // 10/1000     expenses are O(1,000)
  // 100/2000    expenses are O(100)
  // 1,000/2000  expenses are O(10)
  // 10,000/2000 expenses are O(1)
  const r = Math.random();

  if (r <= 0.02) {
    return Math.floor(Math.random() * 5000);
  } else if (r <= 0.1) {
    return Math.floor(Math.random() * 1000);
  } else if (r <= 0.5) {
    return Math.floor(Math.random() * 100);
  } else {
    return Math.floor(Math.random() * 10);
  }
}

// Browser starts to choke around 10,000 data points.
function generateData() {
  return Array(2000).fill(0).map(x => ({
    // select a random day [0, 365]
    x: Math.floor(Math.random() * 365),
    // select a random USD amount in the range [1, 5,000]
    y: randomExpense(),
    // TODO(wpcarro): Attach transaction to `metadata` key.
    metadata: { foo: 'bar' },
  }));
}

////////////////////////////////////////////////////////////////////////////////
// Main
////////////////////////////////////////////////////////////////////////////////

const mount = document.getElementById('mount');

new Chart(mount, {
  type: 'scatter',
  data: {
    datasets: [
      {
        label: 'Expenses',
        data: generateData(),
        backgroundColor: colors.red,
      }
    ],
  },
  options: {
    plugins: {
      tooltip: {
        callbacks: {
          title: function(x) {
            return `$${x[0].raw.y}`;
          },
          label: function(x) {
            return JSON.stringify(x.raw.metadata);
          },
        },
      },
    },
  },
})
