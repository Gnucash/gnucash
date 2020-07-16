module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { from: 'a', to: 'b', flow: 10 },
            { from: 'a', to: 'c', flow: 5 },
            { from: 'a', to: 'd', flow: 10 },
            { from: 'e', to: 'c', flow: 7 },
            { from: 'c', to: 'f', flow: 7 },
            { from: 'c', to: 'g', flow: 7 },
            { from: 'd', to: 'h', flow: 7 },
            { from: 'h', to: 'i', flow: 7 },
          ],
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
