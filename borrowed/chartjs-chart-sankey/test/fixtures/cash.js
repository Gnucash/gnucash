module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { from: 'division a', to: 'revenue', flow: 1150 },
            { from: 'dibision b', to: 'revenue', flow: 650 },
            { from: 'revenue', to: 'cost of sales', flow: 800 },
            { from: 'revenue', to: 'cross profit', flow: 1000 },
            { from: 'cross profit', to: 'operating expenses', flow: 650 },
            { from: 'cross profit', to: 'operating profit', flow: 350 },
          ],
          colorFrom: 'green',
          colorTo: 'gray',
          nodePadding: 100,
          modeX: 'even',
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
