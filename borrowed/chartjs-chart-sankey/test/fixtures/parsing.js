module.exports = {
  threshold: 0.15,
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { source: 'a', destination: 'b', value: 20 },
            { source: 'c', destination: 'd', value: 10 },
            { source: 'c', destination: 'e', value: 5 },
          ],
          colorFrom: 'red',
          colorTo: 'green',
        },
      ],
    },
    options: {
      parsing: {
        from: 'source',
        to: 'destination',
        flow: 'value',
      },
    },
  },
  options: {
    canvas: {
      height: 256,
      width: 512,
    },
  },
}
