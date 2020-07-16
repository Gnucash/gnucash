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

const priority = {
  Oil: 1,
  'Natural Gas': 2,
  Coal: 3,
  'Fossil Fuels': 1,
  Electricity: 2,
  Energy: 1,
}

function getColor(name) {
  return colors[name] || 'green'
}

module.exports = {
  threshold: 0.15,
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data,
          colorFrom: (c) => getColor(c.dataset.data[c.dataIndex].from),
          colorTo: (c) => getColor(c.dataset.data[c.dataIndex].to),
          priority,
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
