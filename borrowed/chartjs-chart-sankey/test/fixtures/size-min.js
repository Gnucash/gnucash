module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { from: 'a1', to: 'b1', flow: 30 },
            { from: 'b1', to: 'c1', flow: 20 },
            { from: 'b1', to: 'c2', flow: 20 },
            { from: 'b1', to: 'c3', flow: 20 },
            { from: 'a2', to: 'b2', flow: 20 },
            { from: 'a3', to: 'b2', flow: 20 },
            { from: 'a4', to: 'b2', flow: 20 },
            { from: 'a5', to: 'b2', flow: 20 },
            { from: 'b2', to: 'c4', flow: 30 },
          ],
          colorFrom: 'red',
          colorTo: 'green',
          size: 'min',
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
