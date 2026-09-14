module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { from: 'TEST1', to: 'TEST2', flow: 15 },
            { from: 'TEST3', to: 'TEST2', flow: 20 },
            { from: 'TEST4', to: 'TEST2', flow: 25 },
            { from: 'TEST4', to: 'TEST5', flow: 25 },
            { from: 'TEST2', to: 'TEST6', flow: 60 },
            { from: 'TEST5', to: 'TEST6', flow: 25 },
            { from: 'TEST7', to: 'TEST6', flow: 25 },
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
      height: 256,
      width: 512,
    },
  },
}
