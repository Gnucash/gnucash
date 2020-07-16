module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { from: 'a', to: 'b', flow: 20 },
            { from: 'b', to: 'c', flow: 20 },
          ],
          colorFrom: 'red',
          colorTo: 'green',
        },
      ],
    },
  },
  options: {
    canvas: {
      height: 256,
      width: 512,
    },
  },
}
