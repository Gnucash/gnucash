module.exports = {
  threshold: 0.15,
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { from: 'a', to: 'b', flow: 20 },
            { from: 'c', to: 'd', flow: 10 },
            { from: 'c', to: 'e', flow: 5 },
          ],
          colorFrom: 'red',
          colorTo: 'green',
          borderColor: 'blue',
          borderWidth: 5,
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
