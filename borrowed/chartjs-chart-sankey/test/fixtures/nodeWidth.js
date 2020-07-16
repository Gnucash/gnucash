module.exports = {
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
          nodeWidth: 50,
        },
      ],
    },
    options: {
      layout: {
        padding: {
          right: 53,
        },
      },
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
