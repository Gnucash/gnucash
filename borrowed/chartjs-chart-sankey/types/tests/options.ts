import { Chart } from 'chart.js'

import '../index.esm'

const colors2 = ['#fff5eb', '#fee6ce', '#fdd0a2', '#fdae6b', '#fd8d3c', '#f16913', '#d94801', '#a63603', '#7f2704']
const assigned: Record<string, string> = {}

function getColor(name: string): string {
  return assigned[name] || (assigned[name] = colors2[Object.keys(assigned).length % colors2.length])
}

const chart = new Chart('test', {
  type: 'sankey',
  data: {
    datasets: [
      {
        label: 'My sankey',
        data: [
          { from: 'a', to: 'b', flow: 10 },
          { from: 'a', to: 'c', flow: 5 },
          { from: 'b', to: 'c', flow: 10 },
        ],
        colorFrom: (c) => getColor(c.dataset.data[c.dataIndex].from),
        colorTo: (c) => getColor(c.dataset.data[c.dataIndex].to),
        colorMode: 'gradient', // or 'from' or 'to'
        /* optional labels */
        labels: {
          a: 'Label A',
          b: 'Label B',
          c: 'Label C',
        },
      },
    ],
  },
})
