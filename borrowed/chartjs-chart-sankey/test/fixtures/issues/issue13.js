module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { from: '1', to: 'Alpha', flow: 1 },
            { from: '2', to: 'Beta', flow: 1 },
            { from: '3', to: 'Gamma', flow: 1 },
            { from: '4', to: 'Delta', flow: 1 },
            { from: '5', to: 'Gamma', flow: 1 },
          ],
          colorFrom: 'red',
          colorTo: 'green',
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
