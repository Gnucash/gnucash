module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { from: 'a', to: 'b', flow: 8 },
            { from: 'b', to: 'c', flow: 9 },
            { from: 'b', to: 'd', flow: 1 },
            { from: 'a', to: 'e', flow: 7 },
            { from: 'e', to: 'f', flow: 10 },
            { from: 'e', to: 'g', flow: 5 },
          ],
          colorFrom: 'red',
          colorTo: 'green',
          size: 'max',
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
