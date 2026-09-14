const data = [
  { from: 'Oil', to: 'Fossil Fuels', flow: 15 },
  { from: 'Natural Gas', to: 'Fossil Fuels', flow: 20 },
  { from: 'Coal', to: 'Fossil Fuels', flow: 25 },
  { from: 'Coal', to: 'Electricity', flow: 25 },
  { from: 'Fossil Fuels', to: 'Energy', flow: 60 },
  { from: 'Electricity', to: 'Energy', flow: 25 },
]

const colors = {
  Oil: 'black',
  Coal: 'gray',
  'Fossil Fuels': 'slategray',
  Electricity: 'blue',
  Energy: 'orange',
}

const labels = {
  'Natural Gas': 'Nat. gas',
  'Fossil Fuels': 'Fossil',
}

function getColor(name) {
  return colors[name] || 'green'
}

module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data,
          colorFrom: (c) => getColor(c.dataset.data[c.dataIndex].from),
          colorTo: (c) => getColor(c.dataset.data[c.dataIndex].to),
          labels,
        },
      ],
    },
  },
  options: {
    spriteText: true,
    canvas: {
      height: 256,
      width: 512,
    },
  },
}
