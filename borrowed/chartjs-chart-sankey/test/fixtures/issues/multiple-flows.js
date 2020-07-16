module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { from: 'A', to: 'B', flow: 516 },
            { from: 'A', to: 'C', flow: 220 },
            { from: 'A', to: 'D', flow: 64 },
            { from: 'A', to: 'B', flow: 50 },
          ],
          colorFrom: 'blue',
          colorTo: 'red',
        },
      ],
    },
  },
  options: {
    spriteText: true,
    canvas: {
      height: 256,
      width: 256,
    },
  },
}
