const colors = {
  a: 'red',
  b: 'green',
  c: 'blue',
  d: 'gray',
  e: 'yellow',
  f: 'black',
  g: 'white',
  h: 'brown',
}

const getColor = (key) => colors[key]

module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            // { from: 'a', to: 'b', flow: 10 },
            { from: 'a', to: 'c', flow: 5 },
            // { from: 'a', to: 'd', flow: 10 },
            { from: 'd', to: 'e', flow: 5 },
            // { from: 'd', to: 'f', flow: 5 },
            // { from: 'a', to: 'g', flow: 0.0001 },
            { from: 'a', to: 'h', flow: 0 },
          ],
          colorFrom: (c) => getColor(c.dataset.data[c.dataIndex].from),
          colorTo: (c) => getColor(c.dataset.data[c.dataIndex].to),
          colorMode: 'gradient',
          nodePadding: 50,

          labels: {
            a: 'A',
            b: 'B',
            c: 'C',
            d: 'D',
            e: 'DA',
            f: 'DB',
            g: 'NOTOVERLAPPING',
            h: 'BUG',
          },
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
