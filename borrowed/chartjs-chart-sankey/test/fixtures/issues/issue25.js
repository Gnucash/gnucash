const years = ['2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010']

const zones = [1, 2, 3, 4, 5]

let labels = {}

let priority = {}
years.forEach((year) =>
  zones.forEach((zone) => {
    priority[`${year}_${zone}`] = zone
  })
)

const data = [
  {
    from: '2000_1',
    to: '2001_2',
    flow: 17,
  },
  {
    from: '2000_1',
    to: '2001_3',
    flow: 2,
  },
  {
    from: '2000_1',
    to: '2001_4',
    flow: 4,
  },
  {
    from: '2000_1',
    to: '2001_5',
    flow: 0,
  },
  {
    from: '2000_2',
    to: '2001_1',
    flow: 9,
  },
  {
    from: '2000_2',
    to: '2001_3',
    flow: 1,
  },
  {
    from: '2000_2',
    to: '2001_4',
    flow: 0,
  },
  {
    from: '2000_2',
    to: '2001_5',
    flow: 1,
  },
  {
    from: '2000_3',
    to: '2001_1',
    flow: 2,
  },
  {
    from: '2000_3',
    to: '2001_2',
    flow: 1,
  },
  {
    from: '2000_3',
    to: '2001_4',
    flow: 0,
  },
  {
    from: '2000_3',
    to: '2001_5',
    flow: 0,
  },
  {
    from: '2000_4',
    to: '2001_1',
    flow: 1,
  },
  {
    from: '2000_4',
    to: '2001_2',
    flow: 0,
  },
  {
    from: '2000_4',
    to: '2001_3',
    flow: 0,
  },
  {
    from: '2000_4',
    to: '2001_5',
    flow: 0,
  },
  {
    from: '2000_5',
    to: '2001_1',
    flow: 1,
  },
  {
    from: '2000_5',
    to: '2001_2',
    flow: 0,
  },
  {
    from: '2000_5',
    to: '2001_3',
    flow: 0,
  },
  {
    from: '2000_5',
    to: '2001_4',
    flow: 1,
  },
  {
    from: '2001_1',
    to: '2002_2',
    flow: 14,
  },
  {
    from: '2001_1',
    to: '2002_3',
    flow: 4,
  },
  {
    from: '2001_1',
    to: '2002_4',
    flow: 4,
  },
  {
    from: '2001_1',
    to: '2002_5',
    flow: 1,
  },
  {
    from: '2001_2',
    to: '2002_1',
    flow: 18,
  },
  {
    from: '2001_2',
    to: '2002_3',
    flow: 0,
  },
  {
    from: '2001_2',
    to: '2002_4',
    flow: 0,
  },
  {
    from: '2001_2',
    to: '2002_5',
    flow: 0,
  },
  {
    from: '2001_3',
    to: '2002_1',
    flow: 1,
  },
  {
    from: '2001_3',
    to: '2002_2',
    flow: 0,
  },
  {
    from: '2001_3',
    to: '2002_4',
    flow: 0,
  },
  {
    from: '2001_3',
    to: '2002_5',
    flow: 0,
  },
  {
    from: '2001_4',
    to: '2002_1',
    flow: 3,
  },
  {
    from: '2001_4',
    to: '2002_2',
    flow: 0,
  },
  {
    from: '2001_4',
    to: '2002_3',
    flow: 0,
  },
  {
    from: '2001_4',
    to: '2002_5',
    flow: 2,
  },
  {
    from: '2001_5',
    to: '2002_1',
    flow: 2,
  },
  {
    from: '2001_5',
    to: '2002_2',
    flow: 0,
  },
  {
    from: '2001_5',
    to: '2002_3',
    flow: 0,
  },
  {
    from: '2001_5',
    to: '2002_4',
    flow: 3,
  },

  {
    from: '2002_1',
    to: '2003_2',
    flow: 11,
  },
  {
    from: '2002_1',
    to: '2003_3',
    flow: 2,
  },
  {
    from: '2002_1',
    to: '2003_4',
    flow: 5,
  },
  {
    from: '2002_1',
    to: '2003_5',
    flow: 0,
  },
  {
    from: '2002_2',
    to: '2003_1',
    flow: 17,
  },
  {
    from: '2002_2',
    to: '2003_3',
    flow: 1,
  },
  {
    from: '2002_2',
    to: '2003_4',
    flow: 0,
  },
  {
    from: '2002_2',
    to: '2003_5',
    flow: 0,
  },
  {
    from: '2002_3',
    to: '2003_1',
    flow: 5,
  },
  {
    from: '2002_3',
    to: '2003_2',
    flow: 1,
  },
  {
    from: '2002_3',
    to: '2003_4',
    flow: 0,
  },
  {
    from: '2002_3',
    to: '2003_5',
    flow: 0,
  },
  {
    from: '2002_4',
    to: '2003_1',
    flow: 3,
  },
  {
    from: '2002_4',
    to: '2003_2',
    flow: 1,
  },
  {
    from: '2002_4',
    to: '2003_3',
    flow: 0,
  },
  {
    from: '2002_4',
    to: '2003_5',
    flow: 2,
  },
  {
    from: '2002_5',
    to: '2003_1',
    flow: 2,
  },
  {
    from: '2002_5',
    to: '2003_2',
    flow: 0,
  },
  {
    from: '2002_5',
    to: '2003_3',
    flow: 0,
  },
  {
    from: '2002_5',
    to: '2003_4',
    flow: 0,
  },
  {
    from: '2003_1',
    to: '2004_2',
    flow: 16,
  },
  {
    from: '2003_1',
    to: '2004_3',
    flow: 0,
  },
  {
    from: '2003_1',
    to: '2004_4',
    flow: 3,
  },
  {
    from: '2003_1',
    to: '2004_5',
    flow: 2,
  },
  {
    from: '2003_2',
    to: '2004_1',
    flow: 18,
  },
  {
    from: '2003_2',
    to: '2004_3',
    flow: 1,
  },
  {
    from: '2003_2',
    to: '2004_4',
    flow: 0,
  },
  {
    from: '2003_2',
    to: '2004_5',
    flow: 0,
  },
  {
    from: '2003_3',
    to: '2004_1',
    flow: 0,
  },
  {
    from: '2003_3',
    to: '2004_2',
    flow: 0,
  },
  {
    from: '2003_3',
    to: '2004_4',
    flow: 0,
  },
  {
    from: '2003_3',
    to: '2004_5',
    flow: 0,
  },
  {
    from: '2003_4',
    to: '2004_1',
    flow: 5,
  },
  {
    from: '2003_4',
    to: '2004_2',
    flow: 0,
  },
  {
    from: '2003_4',
    to: '2004_3',
    flow: 1,
  },
  {
    from: '2003_4',
    to: '2004_5',
    flow: 1,
  },
  {
    from: '2003_5',
    to: '2004_1',
    flow: 1,
  },
  {
    from: '2003_5',
    to: '2004_2',
    flow: 0,
  },
  {
    from: '2003_5',
    to: '2004_3',
    flow: 0,
  },
  {
    from: '2003_5',
    to: '2004_4',
    flow: 2,
  },
  {
    from: '2004_1',
    to: '2005_2',
    flow: 13,
  },
  {
    from: '2004_1',
    to: '2005_3',
    flow: 3,
  },
  {
    from: '2004_1',
    to: '2005_4',
    flow: 3,
  },
  {
    from: '2004_1',
    to: '2005_5',
    flow: 1,
  },
  {
    from: '2004_2',
    to: '2005_1',
    flow: 15,
  },
  {
    from: '2004_2',
    to: '2005_3',
    flow: 1,
  },
  {
    from: '2004_2',
    to: '2005_4',
    flow: 0,
  },
  {
    from: '2004_2',
    to: '2005_5',
    flow: 0,
  },
  {
    from: '2004_3',
    to: '2005_1',
    flow: 2,
  },
  {
    from: '2004_3',
    to: '2005_2',
    flow: 0,
  },
  {
    from: '2004_3',
    to: '2005_4',
    flow: 0,
  },
  {
    from: '2004_3',
    to: '2005_5',
    flow: 0,
  },
  {
    from: '2004_4',
    to: '2005_1',
    flow: 6,
  },
  {
    from: '2004_4',
    to: '2005_2',
    flow: 0,
  },
  {
    from: '2004_4',
    to: '2005_3',
    flow: 1,
  },
  {
    from: '2004_4',
    to: '2005_5',
    flow: 2,
  },
  {
    from: '2004_5',
    to: '2005_1',
    flow: 1,
  },
  {
    from: '2004_5',
    to: '2005_2',
    flow: 0,
  },
  {
    from: '2004_5',
    to: '2005_3',
    flow: 0,
  },
  {
    from: '2004_5',
    to: '2005_4',
    flow: 3,
  },
  {
    from: '2005_1',
    to: '2006_2',
    flow: 1,
  },
  {
    from: '2005_1',
    to: '2006_3',
    flow: 0,
  },
  {
    from: '2005_1',
    to: '2006_4',
    flow: 1,
  },
  {
    from: '2005_1',
    to: '2006_5',
    flow: 0,
  },
  {
    from: '2005_2',
    to: '2006_1',
    flow: 2,
  },
  {
    from: '2005_2',
    to: '2006_3',
    flow: 0,
  },
  {
    from: '2005_2',
    to: '2006_4',
    flow: 0,
  },
  {
    from: '2005_2',
    to: '2006_5',
    flow: 0,
  },
  {
    from: '2005_3',
    to: '2006_1',
    flow: 0,
  },
  {
    from: '2005_3',
    to: '2006_2',
    flow: 0,
  },
  {
    from: '2005_3',
    to: '2006_4',
    flow: 0,
  },
  {
    from: '2005_3',
    to: '2006_5',
    flow: 0,
  },
  {
    from: '2005_4',
    to: '2006_1',
    flow: 1,
  },
  {
    from: '2005_4',
    to: '2006_2',
    flow: 0,
  },
  {
    from: '2005_4',
    to: '2006_3',
    flow: 0,
  },
  {
    from: '2005_4',
    to: '2006_5',
    flow: 1,
  },
  {
    from: '2005_5',
    to: '2006_1',
    flow: 0,
  },
  {
    from: '2005_5',
    to: '2006_2',
    flow: 0,
  },
  {
    from: '2005_5',
    to: '2006_3',
    flow: 0,
  },
  {
    from: '2005_5',
    to: '2006_4',
    flow: 0,
  },
  {
    from: '2006_1',
    to: '2007_2',
    flow: 79,
  },
  {
    from: '2006_1',
    to: '2007_3',
    flow: 11,
  },
  {
    from: '2006_1',
    to: '2007_4',
    flow: 28,
  },
  {
    from: '2006_1',
    to: '2007_5',
    flow: 7,
  },
  {
    from: '2006_2',
    to: '2007_1',
    flow: 58,
  },
  {
    from: '2006_2',
    to: '2007_3',
    flow: 3,
  },
  {
    from: '2006_2',
    to: '2007_4',
    flow: 3,
  },
  {
    from: '2006_2',
    to: '2007_5',
    flow: 0,
  },
  {
    from: '2006_3',
    to: '2007_1',
    flow: 8,
  },
  {
    from: '2006_3',
    to: '2007_2',
    flow: 0,
  },
  {
    from: '2006_3',
    to: '2007_4',
    flow: 0,
  },
  {
    from: '2006_3',
    to: '2007_5',
    flow: 0,
  },
  {
    from: '2006_4',
    to: '2007_1',
    flow: 17,
  },
  {
    from: '2006_4',
    to: '2007_2',
    flow: 0,
  },
  {
    from: '2006_4',
    to: '2007_3',
    flow: 0,
  },
  {
    from: '2006_4',
    to: '2007_5',
    flow: 5,
  },
  {
    from: '2006_5',
    to: '2007_1',
    flow: 2,
  },
  {
    from: '2006_5',
    to: '2007_2',
    flow: 1,
  },
  {
    from: '2006_5',
    to: '2007_3',
    flow: 0,
  },
  {
    from: '2006_5',
    to: '2007_4',
    flow: 6,
  },
  {
    from: '2007_1',
    to: '2008_2',
    flow: 19,
  },
  {
    from: '2007_1',
    to: '2008_3',
    flow: 3,
  },
  {
    from: '2007_1',
    to: '2008_4',
    flow: 9,
  },
  {
    from: '2007_1',
    to: '2008_5',
    flow: 1,
  },
  {
    from: '2007_2',
    to: '2008_1',
    flow: 19,
  },
  {
    from: '2007_2',
    to: '2008_3',
    flow: 0,
  },
  {
    from: '2007_2',
    to: '2008_4',
    flow: 1,
  },
  {
    from: '2007_2',
    to: '2008_5',
    flow: 0,
  },
  {
    from: '2007_3',
    to: '2008_1',
    flow: 2,
  },
  {
    from: '2007_3',
    to: '2008_2',
    flow: 1,
  },
  {
    from: '2007_3',
    to: '2008_4',
    flow: 0,
  },
  {
    from: '2007_3',
    to: '2008_5',
    flow: 0,
  },
  {
    from: '2007_4',
    to: '2008_1',
    flow: 3,
  },
  {
    from: '2007_4',
    to: '2008_2',
    flow: 0,
  },
  {
    from: '2007_4',
    to: '2008_3',
    flow: 0,
  },
  {
    from: '2007_4',
    to: '2008_5',
    flow: 4,
  },
  {
    from: '2007_5',
    to: '2008_1',
    flow: 4,
  },
  {
    from: '2007_5',
    to: '2008_2',
    flow: 0,
  },
  {
    from: '2007_5',
    to: '2008_3',
    flow: 0,
  },
  {
    from: '2007_5',
    to: '2008_4',
    flow: 5,
  },
  {
    from: '2008_1',
    to: '2009_2',
    flow: 22,
  },
  {
    from: '2008_1',
    to: '2009_3',
    flow: 13,
  },
  {
    from: '2008_1',
    to: '2009_4',
    flow: 5,
  },
  {
    from: '2008_1',
    to: '2009_5',
    flow: 3,
  },
  {
    from: '2008_2',
    to: '2009_1',
    flow: 24,
  },
  {
    from: '2008_2',
    to: '2009_3',
    flow: 1,
  },
  {
    from: '2008_2',
    to: '2009_4',
    flow: 0,
  },
  {
    from: '2008_2',
    to: '2009_5',
    flow: 0,
  },
  {
    from: '2008_3',
    to: '2009_1',
    flow: 2,
  },
  {
    from: '2008_3',
    to: '2009_2',
    flow: 1,
  },
  {
    from: '2008_3',
    to: '2009_4',
    flow: 2,
  },
  {
    from: '2008_3',
    to: '2009_5',
    flow: 0,
  },
  {
    from: '2008_4',
    to: '2009_1',
    flow: 5,
  },
  {
    from: '2008_4',
    to: '2009_2',
    flow: 0,
  },
  {
    from: '2008_4',
    to: '2009_3',
    flow: 0,
  },
  {
    from: '2008_4',
    to: '2009_5',
    flow: 3,
  },
  {
    from: '2008_5',
    to: '2009_1',
    flow: 0,
  },
  {
    from: '2008_5',
    to: '2009_2',
    flow: 0,
  },
  {
    from: '2008_5',
    to: '2009_3',
    flow: 0,
  },
  {
    from: '2008_5',
    to: '2009_4',
    flow: 2,
  },
  {
    from: '2009_1',
    to: '2010_2',
    flow: 17,
  },
  {
    from: '2009_1',
    to: '2010_3',
    flow: 4,
  },
  {
    from: '2009_1',
    to: '2010_4',
    flow: 7,
  },
  {
    from: '2009_1',
    to: '2010_5',
    flow: 1,
  },
  {
    from: '2009_2',
    to: '2010_1',
    flow: 7,
  },
  {
    from: '2009_2',
    to: '2010_3',
    flow: 0,
  },
  {
    from: '2009_2',
    to: '2010_4',
    flow: 1,
  },
  {
    from: '2009_2',
    to: '2010_5',
    flow: 0,
  },
  {
    from: '2009_3',
    to: '2010_1',
    flow: 4,
  },
  {
    from: '2009_3',
    to: '2010_2',
    flow: 0,
  },
  {
    from: '2009_3',
    to: '2010_4',
    flow: 0,
  },
  {
    from: '2009_3',
    to: '2010_5',
    flow: 0,
  },
  {
    from: '2009_4',
    to: '2010_1',
    flow: 5,
  },
  {
    from: '2009_4',
    to: '2010_2',
    flow: 0,
  },
  {
    from: '2009_4',
    to: '2010_3',
    flow: 1,
  },
  {
    from: '2009_4',
    to: '2010_5',
    flow: 2,
  },
  {
    from: '2009_5',
    to: '2010_1',
    flow: 0,
  },
  {
    from: '2009_5',
    to: '2010_2',
    flow: 0,
  },
  {
    from: '2009_5',
    to: '2010_3',
    flow: 0,
  },
  {
    from: '2009_5',
    to: '2010_4',
    flow: 2,
  },
]

const colors = {
  1: 'gray',
  2: '#f5c400',
  3: '#f48f00',
  4: '#e61e0e',
  5: '#a7120e',
}

const getColor = (nodeId) => colors[nodeId.split('_')[1]]

module.exports = {
  tolerance: 0.02,
  threshold: 0.15,
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data,
          priority,
          labels,
          colorFrom: (c) => getColor(c.dataset.data[c.dataIndex].from),
          colorTo: (c) => getColor(c.dataset.data[c.dataIndex].to),
          borderWidth: 2,
          borderColor: 'black',
        },
      ],
    },
  },
  options: {
    spriteText: true,
    canvas: {
      width: 1280,
      height: 960,
    },
  },
}
