const colors = {
  a: 'red',
  b: 'green',
  c: 'blue',
  d: 'gray',
  e: 'yellow',
  f: 'black',
  g: 'white',
  h: 'brown',
}

const getColor = (key) => colors[key]

module.exports = {
  tolerance: 0.02,
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { from: 'a', to: 'b', flow: 10 },
            { from: 'a', to: 'c', flow: 5 },
            { from: 'a', to: 'd', flow: 10 },
            { from: 'd', to: 'e', flow: 5 },
            { from: 'd', to: 'f', flow: 5 },
            // { from: 'a', to: 'g', flow: 0.0001 },
            { from: 'a', to: 'h', flow: 0 },
            { from: 'h', to: 'i', flow: 0 },
          ],
          colorFrom: (c) => getColor(c.dataset.data[c.dataIndex].from),
          colorTo: (c) => getColor(c.dataset.data[c.dataIndex].to),
          colorMode: 'gradient',
          nodePadding: 50,
        },
      ],
    },
  },
  options: {
    canvas: {
      height: 256,
      width: 512,
    },
    spriteText: true,
  },
}
