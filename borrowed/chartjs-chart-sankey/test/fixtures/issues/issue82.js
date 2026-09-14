module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { from: 'Oil', to: 'Energy', flow: 15 },
            { from: 'Natural Gas', to: 'Energy', flow: 20 },
            { from: 'Coal', to: 'Energy', flow: 25 },
            { from: 'Electricity', to: 'Energy', flow: 25 },
          ],
          column: {
            Oil: 0,
            'Natural Gas': 1,
            Coal: 2,
            Electricity: 3,
            Energy: 4,
          },
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
