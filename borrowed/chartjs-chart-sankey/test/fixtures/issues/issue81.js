module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { from: 'Oil', to: 'Air1', flow: 15 },
            { from: 'Oil', to: 'Stuff1', flow: 20 },
            { from: 'Oil1', to: 'Air2', flow: 25 },
            { from: 'Oil1', to: 'Stuff2', flow: 25 },
          ],
          priority: {
            Oil: 1,
            Oil1: 2,
            Air: 3,
            Air2: 1,
            Stuff1: 2,
            Stuff2: 1,
          },
          column: {
            Oil: 0,
            Oil1: 1,
            Air1: 1,
            Air2: 2,
            Stuff1: 1,
            Stuff2: 2,
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
