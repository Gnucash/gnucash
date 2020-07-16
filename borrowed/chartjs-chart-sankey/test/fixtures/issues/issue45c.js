module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { to: 'TEST1', from: 'TEST2', flow: 15 },
            { to: 'TEST3', from: 'TEST2', flow: 20 },
            { to: 'TEST4', from: 'TEST2', flow: 25 },
            { to: 'TEST4', from: 'TEST5', flow: 25 },
            { to: 'TEST2', from: 'TEST6', flow: 60 },
            { to: 'TEST5', from: 'TEST6', flow: 25 },
            { to: 'TEST7', from: 'TEST6', flow: 25 },
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
