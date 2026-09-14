const colors = {
  a: 'blue',
  b: 'green',
  c: 'orange',
  d: 'red',
  e: 'violet',
}

module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { from: 'a', to: 'b', flow: 20 },
            { from: 'b', to: 'c', flow: 10 },
            { from: 'a', to: 'c', flow: 10 },
            { from: 'b', to: 'd', flow: 10 },
            { from: 'c', to: 'e', flow: 20 },
          ],
          colorFrom: (c) => colors[c.dataset.data[c.dataIndex].from],
          colorTo: (c) => colors[c.dataset.data[c.dataIndex].to],
          colorMode: 'gradient',
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
