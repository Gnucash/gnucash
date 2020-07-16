const data = [
  ['Net Import', 'Electricity & Heat', 0.14],
  ['Solar', 'Electricity & Heat', 1.28],
  ['Nuclear', 'Electricity & Heat', 8.05],
  ['Hydro', 'Electricity & Heat', 2.31],
  ['Wind', 'Electricity & Heat', 3.84],
  ['Geothermal', 'Electricity & Heat', 0.15],
  ['Natural Gas', 'Electricity & Heat', 12.5],
  ['Coal', 'Electricity & Heat', 8.9],
  ['Biomass', 'Electricity & Heat', 0.41],
  ['Petroleum', 'Electricity & Heat', 0.24],

  ['Electricity & Heat', 'Residential', 5.19],
  ['Solar', 'Residential', 0.4],
  ['Geothermal', 'Residential', 0.04],
  ['Natural Gas', 'Residential', 5.17],
  ['Biomass', 'Residential', 0.48],
  ['Petroleum', 'Residential', 0.98],

  ['Electricity & Heat', 'Commercial', 4.69],
  ['Solar', 'Commercial', 0.16],
  ['Geothermal', 'Commercial', 0.02],
  ['Natural Gas', 'Commercial', 3.65],
  ['Coal', 'Commercial', 0.02],
  ['Biomass', 'Commercial', 0.15],
  ['Petroleum', 'Commercial', 0.88],

  ['Electricity & Heat', 'Industrial', 3.44],
  ['Solar', 'Industrial', 0.04],
  ['Natural Gas', 'Industrial', 10.8],
  ['Coal', 'Industrial', 0.99],
  ['Biomass', 'Industrial', 2.27],
  ['Petroleum', 'Industrial', 9.13],

  ['Electricity & Heat', 'Transportation', 0.02],
  ['Natural Gas', 'Transportation', 1.29],
  ['Biomass', 'Transportation', 1.57],
  ['Petroleum', 'Transportation', 24.6],

  ['Electricity & Heat', 'Rejected Energy', 24.3],
  ['Residential', 'Rejected Energy', 4.29],
  ['Commercial', 'Rejected Energy', 3.35],
  ['Industrial', 'Rejected Energy', 13.6],
  ['Transportation', 'Rejected Energy', 21.7],

  ['Residential', 'Energy Services', 7.97],
  ['Commercial', 'Energy Services', 6.22],
  ['Industrial', 'Energy Services', 13.1],
  ['Transportation', 'Energy Services', 5.77],
].map(([from, to, flow]) => ({ from, to, flow }))

module.exports = {
  tolerance: 0.02,
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data,
          colorFrom: 'red',
          colorTo: 'blue',
          color: 'transparent',
        },
      ],
    },
  },
  options: {
    canvas: {
      height: 750,
      width: 1000,
    },
    spriteText: true,
  },
}
