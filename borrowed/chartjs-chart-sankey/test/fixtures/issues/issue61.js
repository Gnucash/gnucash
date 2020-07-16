module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { from: '1', to: 'Alpha', flow: 1 },
            { from: '2', to: 'Beta', flow: 1 },
            { from: '3', to: 'Gamma', flow: 4 },
            { from: '5', to: 'Gamma', flow: 1 },
          ],
          colorFrom: '#f00',
          colorTo: '#008000',
          hoverColorFrom: '#800',
          hoverColorTo: '#005000',
        },
      ],
    },
    options: {
      events: [], // disable events so it's easier to save the fixture without active elements changing
    },
  },
  options: {
    spriteText: true,
    canvas: {
      height: 512,
      width: 512,
    },
    run(chart) {
      chart.setActiveElements([{ datasetIndex: 0, index: 2 }])
      chart.draw()
    },
  },
}
