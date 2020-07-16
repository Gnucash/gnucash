const colors = {
  AA: 'red',
  BB: 'green',
  CC: 'blue',
  DD: 'gray',
  EE: 'yellow',
  XX: 'black',
  YY: 'white',
  '115 ': 'brown',
  '110 ': 'magenta',
  '29  ': 'navy',
}

const getColor = (key) => colors[key]

module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            {
              from: '115 ',
              to: 'AA',
              flow: 5,
            },
            {
              from: '115 ',
              to: 'BB',
              flow: 4,
            },
            {
              from: '115 ',
              to: 'CC',
              flow: 4,
            },
            {
              from: '29  ',
              to: 'DD',
              flow: 4,
            },
            {
              from: '110 ',
              to: 'EE',
              flow: 3,
            },
            {
              from: 'AA',
              to: 'XX',
              flow: 5,
            },
            {
              from: 'BB',
              to: 'XX',
              flow: 4,
            },
            {
              from: 'CC',
              to: 'YY',
              flow: 4,
            },
            {
              from: 'DD',
              to: 'XX',
              flow: 4,
            },
            {
              from: 'EE',
              to: 'XX',
              flow: 3,
            },
          ],
          colorFrom: (c) => getColor(c.dataset.data[c.dataIndex].from),
          colorTo: (c) => getColor(c.dataset.data[c.dataIndex].to),
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
