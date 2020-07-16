const data = [
  { from: 'Oil', to: 'Fossil Fuels', flow: 100 },
  { from: 'Oil', to: 'Natural Gas', flow: 50 },
  { from: 'Natural Gas', to: 'Coal', flow: 50 },
  { from: 'Coal', to: 'Electricity', flow: 50 },
  { from: 'Fossil Fuels', to: 'Energy', flow: 100 },
]

const colors = {
  Oil: 'black',
  Coal: 'gray',
  'Fossil Fuels': 'slategray',
  Electricity: 'blue',
  Energy: 'orange',
}

const labels = {
  'Natural Gas': 'Nat. gas \nsubtitle 1\nsubtitle 2',
  'Fossil Fuels': 'Fossil\nsubtitle 1\nsubtitle 2',
  Coal: 'Coal\nsubtitle 1\nsubtitle 2\nsubtitle 3\nsubtitle 4',
  Energy: 'Energy\nsubtitle 1\nsubtitle 2\nsubtitle 3',
  Electricity: 'Electricity\nsubtitle 1',
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
      height: 512,
      width: 512,
    },
  },
}
